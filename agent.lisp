(in-package #:yuuki)

(defun user-item (text)
  (obj "role" "user" "content" (vector (obj "type" "input_text" "text" text))))

(defun output-item (call output)
  (obj "type" "function_call_output" "call_id" (gethash "call_id" call) "output" output))

(defun calls (items)
  (remove-if-not (lambda (item) (equal (gethash "type" item) "function_call")) items))

(defparameter *review-tool*
  (obj "type" "function" "name" "permission_decision" "strict" nil
       "description" "Decide whether the pending Lisp code may run."
       "parameters" (obj "type" "object"
                         "properties" (obj "decision" (obj "type" "string" "enum" (vector "clear" "caution"))
                                           "rationale" (obj "type" "string"))
                         "required" (vector "decision" "rationale"))))

(defparameter *review-prompt*
  "You review one pending action of a coding agent that evaluates Common Lisp in the user's workspace. Given the user's request and the code about to run, answer clear when the code is a routine, reversible development action that plainly serves the request: reading files, searching, building, testing, editing files inside the workspace, defining functions. Answer caution when it deletes or overwrites outside the workspace, touches credentials or secrets, pushes, resets or discards changes, sends data to unexpected destinations, or does anything the request did not ask for. When unsure, answer caution. Keep the rationale to one sentence.")

(defun review-call (request code &key (stream #'stream-turn))
  "Ask the model whether CODE may run for REQUEST. Returns :clear or :caution and a
rationale; caution on any failure or malformed reply."
  (handler-case
      (let* ((text (format nil "User request:~%~A~%~%Pending code:~%~A" request code))
             (output (funcall stream *review-prompt* (list (user-item text))
                              (lambda (event) (declare (ignore event)))
                              :tools (vector *review-tool*)
                              :tool-choice (obj "type" "function" "name" "permission_decision")
                              :effort "low"))
             (call (find "permission_decision" (calls output)
                         :key (lambda (call) (gethash "name" call)) :test #'equal))
             (arguments (and call (com.inuoe.jzon:parse (gethash "arguments" call)))))
        (values (if (equal (path arguments "decision") "clear") :clear :caution)
                (or (path arguments "rationale") "")))
    (cancelled (condition) (error condition))
    (error (condition) (values :caution (format nil "review failed: ~A" condition)))))

(defun run-call (call emit approve)
  "Announce, approve and run one function_call; returns its function_call_output item."
  (let* ((id (gethash "call_id" call))
         (arguments (handler-case (com.inuoe.jzon:parse (gethash "arguments" call))
                      (error () (obj "code" ""))))
         (code (or (gethash "code" arguments) ""))
         (timeout (let ((value (gethash "timeout" arguments)))
                    (if (realp value) value 60))))
    (funcall emit (list :call id code))
    (let ((output (if (funcall approve id code)
                      (run-lisp code :timeout timeout)
                      "denied by user")))
      (funcall emit (list :result id output))
      (output-item call output))))

(defun assistant-item (text)
  (obj "type" "message" "role" "assistant" "status" "completed"
       "content" (vector (obj "type" "output_text" "text" text "annotations" (vector)))))

(defun output-text (items)
  "The text of the message items among ITEMS."
  (with-output-to-string (out)
    (dolist (item items)
      (when (equal (gethash "type" item) "message")
        (loop for part across (or (gethash "content" item) (vector))
              do (write-string (or (and (hash-table-p part) (gethash "text" part)) "") out))))))

(defun step-item (task state observation)
  "The one user item a step sees: the task, the agent's state, and the latest observation."
  (user-item (format nil "<task>~%~A~%</task>~%~%<state>~%~A~%</state>~%~%<observation>~%~A~%</observation>"
                     task state observation)))

(defun run-turn (history prompt &key emit approve (stream #'stream-turn))
  "One turn of state-centric execution. Each step's request is HISTORY (user prompts and
final answers of earlier turns) plus one item carrying PROMPT, the agent's definitions
with their values, and the code run last with its result. Earlier steps of this turn are
never replayed. Returns HISTORY plus the user item and the final answer; on cancel,
HISTORY unchanged. EMIT receives events; APPROVE decides each call."
  (let ((observation "none yet: this is the first step"))
    (loop repeat *max-steps*
          do (multiple-value-bind (output finish)
                 (funcall stream (instructions)
                          (append history (list (step-item prompt (state-block) observation)))
                          emit)
               (let ((calls (calls output)))
                 (when *cancel* (return history))
                 (when (or (null calls) (not (eq finish :stop)))
                   (let ((text (output-text output)))
                     (return (append history (list (user-item prompt))
                                     (when (plusp (length text)) (list (assistant-item text)))))))
                 (setf observation
                       (format nil "~{~A~^~%~%~}"
                               (loop for call in calls
                                     until *cancel*
                                     collect (let ((result (run-call call emit approve)))
                                               (format nil "~A~%~A"
                                                       (or (path (ignore-errors (com.inuoe.jzon:parse (gethash "arguments" call))) "code") "")
                                                       (gethash "output" result))))))
                 (when *cancel* (return history))))
          finally (funcall emit (list :error (format nil "step limit of ~D reached~%" *max-steps*)))
                  (return (append history (list (user-item prompt)))))))