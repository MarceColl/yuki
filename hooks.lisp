(in-package #:yuuki)

(defparameter *hook-events*
  '(:turn-start :before-model :after-model :before-tool :after-tool :turn-end)
  "Lifecycle events to which functions may be attached.")

(defvar *hooks* (make-hash-table :test #'eq)
  "Lifecycle hook bindings, keyed by event.")

(defvar *hooks-lock* (make-lock "hooks")
  "Protects hook registration while the agent and REPL run concurrently.")

(defun check-hook-event (event)
  (unless (member event *hook-events*)
    (error "Unknown hook event ~S; expected one of ~{~S~^, ~}."
           event *hook-events*))
  event)

(defun hook-function-p (value)
  (or (functionp value) (symbolp value)))

(defun add-hook (event function &key (name function))
  "Attach FUNCTION to lifecycle EVENT and return NAME.
FUNCTION receives an event plist. Its nil, string, or list-of-strings result is
added to model context for the rest of the current turn. Reusing NAME replaces
the existing binding without changing its position."
  (check-hook-event event)
  (unless (hook-function-p function)
    (error 'type-error :datum function :expected-type '(or function symbol)))
  (with-lock (*hooks-lock*)
    (let* ((bindings (copy-list (gethash event *hooks*)))
           (position (position name bindings :key #'first :test #'equal))
           (binding (list name function)))
      (if position
          (setf (nth position bindings) binding)
          (setf bindings (append bindings (list binding))))
      (setf (gethash event *hooks*) bindings)))
  name)

(defun remove-hook (event name)
  "Remove the hook named NAME from lifecycle EVENT. Return true when found."
  (check-hook-event event)
  (with-lock (*hooks-lock*)
    (let* ((bindings (gethash event *hooks*))
           (remaining (remove name bindings :key #'first :test #'equal))
           (found (< (length remaining) (length bindings))))
      (if remaining
          (setf (gethash event *hooks*) remaining)
          (remhash event *hooks*))
      found)))

(defun list-hooks (&optional event)
  "List registered hooks as (NAME FUNCTION) pairs.
With EVENT, return that event's bindings. Without it, return (EVENT . BINDINGS)
for every lifecycle event that has bindings."
  (when event (check-hook-event event))
  (with-lock (*hooks-lock*)
    (if event
        (copy-tree (gethash event *hooks*))
        (loop for kind in *hook-events*
              for bindings = (gethash kind *hooks*)
              when bindings collect (cons kind (copy-tree bindings))))))

(defun clear-hooks (&optional event)
  "Remove hooks for EVENT, or every lifecycle hook when EVENT is nil."
  (when event (check-hook-event event))
  (with-lock (*hooks-lock*)
    (if event (remhash event *hooks*) (clrhash *hooks*)))
  nil)

(defun hook-context-values (value)
  (cond ((null value) nil)
        ((stringp value) (list value))
        ((listp value)
         (dolist (item value value)
           (unless (stringp item)
             (error 'type-error :datum item :expected-type 'string))))
        (t (error 'type-error :datum value
                  :expected-type '(or null string list)))))

(defun run-hooks (event details context)
  "Run EVENT's hooks with DETAILS and current CONTEXT, returning context strings."
  (check-hook-event event)
  (let ((bindings (with-lock (*hooks-lock*)
                    (copy-list (gethash event *hooks*))))
        (payload (list* :event event :context (copy-list context) details)))
    (loop for binding in bindings
          append (hook-context-values (funcall (second binding) payload)))))
