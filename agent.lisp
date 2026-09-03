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
             (call (first (calls output)))
             (arguments (and call (com.inuoe.jzon:parse (gethash "arguments" call)))))
        (values (if (equal (path arguments "decision") "clear") :clear :caution)
                (or (path arguments "rationale") "")))
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

(defun run-turn (history prompt &key emit approve (stream #'stream-turn))
  "One turn: HISTORY plus PROMPT, streamed through STREAM, tool calls run until the model stops.
Returns the new history. EMIT receives events; APPROVE decides each call."
  (let ((items (append history (list (user-item prompt)))))
    (loop repeat *max-steps*
          do (multiple-value-bind (output finish) (funcall stream (instructions) items emit)
               (setf items (append items output))
               (let ((calls (calls output)))
                 (when (or (null calls) *cancel* (not (eq finish :stop)))
                   (return items))
                 (setf items (append items (loop for call in calls
                                                 until *cancel*
                                                 collect (run-call call emit approve))))
                 (when *cancel* (return items))))
          finally (return (append items (list (user-item "Step limit reached. Stop and summarize what you did.")))))))
