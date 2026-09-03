(in-package #:yuuki)

(defun user-item (text)
  (obj "role" "user" "content" (vector (obj "type" "input_text" "text" text))))

(defun output-item (call output)
  (obj "type" "function_call_output" "call_id" (gethash "call_id" call) "output" output))

(defun calls (items)
  (remove-if-not (lambda (item) (equal (gethash "type" item) "function_call")) items))

(defun run-call (call emit approve)
  "Announce, approve and run one function_call; returns its function_call_output item."
  (let* ((id (gethash "call_id" call))
         (arguments (handler-case (com.inuoe.jzon:parse (gethash "arguments" call))
                      (error () (obj "code" ""))))
         (code (or (gethash "code" arguments) ""))
         (timeout (or (gethash "timeout" arguments) 60)))
    (funcall emit (list :call id code))
    (let ((output (if (funcall approve id)
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
                 (setf items (append items (mapcar (lambda (call) (run-call call emit approve)) calls)))))
          finally (return (append items (list (user-item "Step limit reached. Stop and summarize what you did.")))))))
