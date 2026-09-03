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
        (values state nil)
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
          (t (values state nil))))
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
                    (values state (list (list :resolve (second args) nil)))
                    (values (update state :phase :approving :approval (second args)) nil)))
      (:done (finish-turn state (first args)))
      (t (values state nil)))))

;;; Rendering.

(defun status-text (state)
  (format nil "~A · ~(~A~) · ~(~A~)~@[ · ~D queued~]"
          *model* (state-phase state) *permission*
          (and (state-queue state) (length (state-queue state)))))

(defun render (out state)
  "Print committed lines to scrollback, repaint the live region, return the state with
committed cleared and live-row updated."
  (let* ((cols (terminal-columns))
         (tail (state-tail state))
         (above (append (when (plusp (length tail)) (list (cons (state-tail-style state) tail)))
                        (list (cons :dim (status-text state)))))
         (approving (eq (state-phase state) :approving))
         (prompt (if approving "run? [y/n]" (format nil "> ~A" (state-composer state))))
         (prefix (if approving prompt (format nil "> ~A" (subseq (state-composer state) 0 (state-cursor state))))))
    (erase out (state-live-row state))
    (dolist (line (state-committed state)) (print-line out line))
    (let ((row (paint out above prompt prefix cols)))
      (finish-output out)
      (update state :committed nil :live-row row))))

;;; Effects.

(defvar *agent* nil "The thread running the current turn, if any.")

(defun start-turn (mailbox history prompt &key (stream #'stream-turn))
  "Run one turn on a fresh thread, posting its events and a final (:done history) to MAILBOX."
  (setf *cancel* nil)
  (flet ((emit (event) (sb-concurrency:send-message mailbox event))
         (approve (id)
           (or (eq *permission* :yolo)
               (let ((reply (sb-concurrency:make-mailbox)))
                 (sb-concurrency:send-message mailbox (list :approve id reply))
                 (sb-concurrency:receive-message reply)))))
    (setf *agent*
          (sb-thread:make-thread
           (lambda ()
             (emit (list :done
                         (handler-case (run-turn history prompt :emit #'emit :approve #'approve :stream stream)
                           (serious-condition (condition)
                             (emit (list :error (format nil "error: ~A~%" condition)))
                             history)))))
           :name "agent"))))

(defun run-effect (effect mailbox)
  (destructuring-bind (kind &rest args) effect
    (case kind
      (:start (start-turn mailbox (first args) (second args)) nil)
      (:resolve (sb-concurrency:send-message (first args) (second args)) nil)
      (:cancel (setf *cancel* t) nil)
      (:eval (sb-concurrency:send-message mailbox (list :result "/" (run-lisp (first args)))) nil)
      (:exit :exit))))

;;; Main.

(defun fx-codex-model ()
  (ignore-errors
   (path (read-json-file (merge-pathnames ".fx/settings.json" (user-homedir-pathname))) "models" "codex")))

(defun configure ()
  "Environment first, then what the image remembers, then fx's settings."
  (let ((permission (sb-ext:posix-getenv "YUUKI_PERMISSION")))
    (setf *model* (or (sb-ext:posix-getenv "YUUKI_MODEL") *model* (fx-codex-model))
          *effort* (or (sb-ext:posix-getenv "YUUKI_EFFORT") *effort*)
          *permission* (if permission (intern (string-upcase permission) '#:keyword) *permission*))))

(defun install-resize (mailbox)
  (sb-sys:enable-interrupt sb-unix:sigwinch
                           (lambda (&rest ignore)
                             (declare (ignore ignore))
                             (sb-concurrency:send-message mailbox '(:resize)))))

(defun drain (mailbox)
  "Block for one event, then take whatever else is already queued."
  (cons (sb-concurrency:receive-message mailbox)
        (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
              while ok collect event)))

(defun main ()
  "Toplevel of the saved image."
  (configure)
  (unless *model*
    (format *error-output* "yuuki: no model; set YUUKI_MODEL or log in with fx~%")
    (sb-ext:exit :code 1))
  (let ((mailbox (sb-concurrency:make-mailbox))
        (state (make-state)))
    (with-terminal (in out)
      (let ((reader (sb-thread:make-thread (lambda () (read-keys in mailbox)) :name "stdin")))
        (install-resize mailbox)
        (unwind-protect
             (block ui
               (setf state (render out state))
               (loop
                 (dolist (event (drain mailbox))
                   (multiple-value-bind (next effects) (reduce-event state event)
                     (setf state next)
                     (dolist (effect effects)
                       (when (eq (run-effect effect mailbox) :exit) (return-from ui)))))
               (setf state (render out state))))
          (setf *cancel* t)
          (when (state-approval state) (sb-concurrency:send-message (state-approval state) nil))
          (when *agent* (sb-thread:join-thread *agent* :default nil :timeout 5))
          (sb-thread:terminate-thread reader)
          (sb-thread:join-thread reader :default nil :timeout 1))))
    (save-image (format nil "~A.new" (namestring sb-ext:*core-pathname*)))))

(defun save-image (path)
  "Save this image to PATH with MAIN as its toplevel. Does not return."
  (dex:clear-connection-pool)
  (setf *agent* nil *cancel* nil)
  (sb-ext:save-lisp-and-die path :toplevel #'main))
