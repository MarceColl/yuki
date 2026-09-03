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
  "Announce, approve and run one function_call. Returns its function_call_output item
and the code it carried, or an empty string when the arguments were malformed."
  (let* ((id (gethash "call_id" call))
         (arguments (ignore-errors (com.inuoe.jzon:parse (gethash "arguments" call))))
         (code (and (hash-table-p arguments)
                    (stringp (gethash "code" arguments))
                    (gethash "code" arguments)))
         (timeout (let ((value (and code (gethash "timeout" arguments))))
                    (if (realp value) value 60))))
    (funcall emit (list :call id (or code "")))
    (let ((output (cond ((null code) "error: malformed arguments, expected {\"code\": string}")
                        ((funcall approve id code) (run-lisp code :timeout timeout))
                        (t "denied by user"))))
      (funcall emit (list :result id output))
      (values (output-item call output) (or code "")))))

(defun merge-hook-context (context additions)
  "Append new hook context strings in order, suppressing exact duplicates."
  (reduce (lambda (result addition)
            (if (member addition result :test #'string=)
                result
                (append result (list addition))))
          additions :initial-value context))

(defun run-turn (history prompt &key emit approve (stream #'stream-turn))
  "One turn: HISTORY plus PROMPT, streamed through STREAM, with lifecycle hooks.
Returns the new history. EMIT receives events; APPROVE decides each call."
  (let ((items (append history (list (user-item prompt))))
        (hook-context nil))
    (labels ((invoke (event &rest details)
               (setf hook-context
                     (merge-hook-context
                      hook-context (run-hooks event details hook-context))))
             (finish-turn (reason)
               (invoke :turn-end :prompt prompt :history items :reason reason)
               items))
      (invoke :turn-start :prompt prompt :history items)
      (loop for step from 1 to *max-steps*
            do (invoke :before-model :prompt prompt :history items :step step)
               (multiple-value-bind (output finish)
                   (funcall stream (instructions hook-context) items emit)
                 (setf items (append items output))
                 (invoke :after-model :prompt prompt :history items :step step
                         :output output :finish finish)
                 (let ((calls (calls output)))
                   (cond (*cancel* (return (finish-turn :cancelled)))
                         ((not (eq finish :stop))
                          (return (finish-turn finish)))
                         ((null calls) (return (finish-turn :complete))))
                   (dolist (call calls)
                     (when *cancel* (return))
                     (let* ((arguments (ignore-errors
                                        (com.inuoe.jzon:parse
                                         (gethash "arguments" call))))
                            (code (and (hash-table-p arguments)
                                       (stringp (gethash "code" arguments))
                                       (gethash "code" arguments))))
                       (invoke :before-tool :prompt prompt :history items :step step
                               :call call :code code)
                       (multiple-value-bind (result actual-code)
                           (run-call call emit approve)
                         (setf items (append items (list result)))
                         (invoke :after-tool :prompt prompt :history items :step step
                                 :call call :code actual-code
                                 :output (gethash "output" result)))))
                   (when *cancel* (return (finish-turn :cancelled)))))
            finally
               (setf items
                     (append items
                             (list (user-item
                                    "Step limit reached. Stop and summarize what you did."))))
               (return (finish-turn :step-limit))))))
