(in-package #:yuuki)

;;; State: immutable in use; every transition returns a copy.

(defstruct state
  (history '())
  (phase :idle)
  (queue '())
  (approval nil)
  (composer "")
  (cursor 0)
  (committed '())
  (tail "")
  (tail-style :plain)
  (live-row 0))

(defun update (state &rest kv)
  "A copy of STATE with slots from the KV plist replaced."
  (let ((copy (copy-state state)))
    (loop for (key value) on kv by #'cddr
          do (case key
               (:history (setf (state-history copy) value))
               (:phase (setf (state-phase copy) value))
               (:queue (setf (state-queue copy) value))
               (:approval (setf (state-approval copy) value))
               (:composer (setf (state-composer copy) value))
               (:cursor (setf (state-cursor copy) value))
               (:committed (setf (state-committed copy) value))
               (:tail (setf (state-tail copy) value))
               (:tail-style (setf (state-tail-style copy) value))
               (:live-row (setf (state-live-row copy) value))))
    copy))

(defun lines (style text)
  (mapcar (lambda (line) (cons style line)) (text-lines text)))

(defun commit (state lines)
  (update state :committed (append (state-committed state) lines)))

(defun flush-tail (state)
  (if (plusp (length (state-tail state)))
      (update (commit state (list (cons (state-tail-style state) (state-tail state))))
              :tail "")
      state))

(defun push-text (state text style)
  "Append TEXT to the tail; complete lines move to committed."
  (let* ((state (if (eq style (state-tail-style state))
                    state
                    (flush-tail state)))
         (parts (text-lines (concatenate 'string (state-tail state) text))))
    (update (commit state
                    (mapcar (lambda (line) (cons style line)) (butlast parts)))
            :tail (car (last parts))
            :tail-style style)))

;;; Keys.

(defun submit (state)
  (let ((text (string-trim " " (state-composer state))))
    (if (zerop (length text))
        (values (update state) nil)
        (let ((state (update state :composer "" :cursor 0)))
          (cond ((char= (char text 0) #\/)
                 (values (commit state (lines :user text))
                         (list (list :eval (subseq text 1)))))
                ((eq (state-phase state) :idle)
                 (values (update (commit state (lines :user text)) :phase :running)
                         (list (list :start (state-history state) text))))
                (t (values (commit (update state
                                           :queue (append (state-queue state)
                                                          (list text)))
                                   (lines :user text))
                           nil)))))))

(defun reduce-key (state key)
  (if (eq (state-phase state) :approving)
      (let ((promise (state-approval state))
            (running (update state :phase :running :approval nil)))
        (case key
          (#\y (values running (list (list :resolve promise t))))
          (#\n (values running (list (list :resolve promise nil))))
          (:ctrl-c (values running (list (list :resolve promise nil)
                                         (list :cancel))))
          (t (values (update state) nil))))
      (case key
        (:ctrl-d (values state (list (list :exit))))
        (:ctrl-c (values state
                         (list (list (if (eq (state-phase state) :running)
                                         :cancel
                                         :exit)))))
        (:enter (submit state))
        (t (multiple-value-bind (text cursor)
               (edit (state-composer state) (state-cursor state) key)
             (values (update state :composer text :cursor cursor) nil))))))

;;; Events.

(defun finish-turn (state history)
  (let ((state (update (commit (flush-tail state) (list (cons :plain "")))
                       :history history
                       :phase :idle)))
    (if (state-queue state)
        (values (update state :queue (rest (state-queue state)) :phase :running)
                (list (list :start history (first (state-queue state)))))
        (values state nil))))

(defun reduce-event (state event)
  "Pure: STATE and EVENT to (values state effects)."
  (destructuring-bind (kind &rest args) event
    (case kind
      (:key (reduce-key state (first args)))
      (:text (values (push-text state (first args) :plain) nil))
      (:reasoning (values (push-text state (first args) :dim) nil))
      (:error (values (push-text state (first args) :error) nil))
      (:call (values (commit (flush-tail state) (lines :code (second args))) nil))
      (:result (values (commit (flush-tail state) (lines :output (second args))) nil))
      (:approve (if (eq (state-phase state) :approving)
                    (values (update state) nil)
                    (values (update state :phase :approving :approval (second args)) nil)))
      (:done (finish-turn state (first args)))
      (t (values (update state) nil)))))
