(in-package #:yuuki)

;;; State: immutable in use; every transition returns a copy.

(defstruct pane
  (log '())          ; (style . text) lines, oldest first
  (composer "")
  (cursor 0)
  (scroll 0))        ; rows scrolled up from the end; 0 follows the tail

(defstruct state
  (history '())
  (phase :idle)      ; :idle | :running | :approving
  (queue '())        ; prompts typed while running
  (approval nil)     ; reply mailbox while approving
  (chat (make-pane))
  (repl (make-pane))
  (focus :chat)      ; :chat | :repl
  (tail "")          ; partial assistant line
  (tail-style :plain))

(defparameter *log-limit* 2000 "Lines kept per pane.")

(defun update (state &rest kv)
  "A copy of STATE with slots from the KV plist replaced."
  (let ((copy (copy-state state)))
    (loop for (key value) on kv by #'cddr
          do (case key
               (:history (setf (state-history copy) value))
               (:phase (setf (state-phase copy) value))
               (:queue (setf (state-queue copy) value))
               (:approval (setf (state-approval copy) value))
               (:chat (setf (state-chat copy) value))
               (:repl (setf (state-repl copy) value))
               (:focus (setf (state-focus copy) value))
               (:tail (setf (state-tail copy) value))
               (:tail-style (setf (state-tail-style copy) value))))
    copy))

(defun pane-update (pane &rest kv)
  (let ((copy (copy-pane pane)))
    (loop for (key value) on kv by #'cddr
          do (case key
               (:log (setf (pane-log copy) value))
               (:composer (setf (pane-composer copy) value))
               (:cursor (setf (pane-cursor copy) value))
               (:scroll (setf (pane-scroll copy) value))))
    copy))

(defun focused (state)
  (if (eq (state-focus state) :chat) (state-chat state) (state-repl state)))

(defun with-focused (state pane)
  (if (eq (state-focus state) :chat) (update state :chat pane) (update state :repl pane)))

(defun append-log (pane lines)
  (let ((log (append (pane-log pane) lines)))
    (pane-update pane :log (if (> (length log) *log-limit*) (last log *log-limit*) log))))

(defun lines (style text)
  (mapcar (lambda (line) (cons style line)) (text-lines text)))

(defun commit (state lines)
  (update state :chat (append-log (state-chat state) lines)))

(defun repl-commit (state lines)
  (update state :repl (append-log (state-repl state) lines)))

(defun flush-tail (state)
  (if (plusp (length (state-tail state)))
      (update (commit state (list (cons (state-tail-style state) (state-tail state))))
              :tail "")
      state))

(defun push-text (state text style)
  "Append TEXT to the tail; complete lines move to the chat log."
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
  "Enter in the chat: start a turn, or queue the prompt while one runs."
  (let ((text (string-trim " " (pane-composer (state-chat state)))))
    (if (zerop (length text))
        (values state nil)
        (let ((state (update state :chat (pane-update (state-chat state) :composer "" :cursor 0))))
          (if (eq (state-phase state) :idle)
              (values (update (commit state (lines :user text)) :phase :running)
                      (list (list :start (state-history state) text)))
              (values (commit (update state :queue (append (state-queue state) (list text)))
                              (lines :user text))
                      nil))))))

(defun repl-submit (state)
  "Enter in the REPL: echo the form and evaluate it on its own thread, always."
  (let ((text (string-trim " " (pane-composer (state-repl state)))))
    (if (zerop (length text))
        (values state nil)
        (values (repl-commit (update state :repl (pane-update (state-repl state) :composer "" :cursor 0))
                             (lines :user (format nil "* ~A" text)))
                (list (list :eval text))))))

(defun scroll-focused (state delta)
  (let ((pane (focused state)))
    (with-focused state (pane-update pane :scroll (max 0 (+ (pane-scroll pane) delta))))))

(defun reduce-key (state key)
  (if (and (eq (state-phase state) :approving) (eq (state-focus state) :chat))
      (let ((promise (state-approval state))
            (running (update state :phase :running :approval nil)))
        (case key
          (#\y (values running (list (list :resolve promise t))))
          (#\n (values running (list (list :resolve promise nil))))
          (:ctrl-c (values running (list (list :resolve promise nil) (list :cancel))))
          (:ctrl-d (values running (list (list :resolve promise nil) (list :exit))))
          (:tab (values (update state :focus :repl) nil))
          (t (values state nil))))
      (case key
        (:ctrl-d (values state (list (list :exit))))
        (:ctrl-c (values state (list (list (if (eq (state-phase state) :running) :cancel :exit)))))
        (:tab (values (update state :focus (if (eq (state-focus state) :chat) :repl :chat)) nil))
        (:page-up (values (scroll-focused state 10) nil))
        (:page-down (values (scroll-focused state -10) nil))
        (:enter (if (eq (state-focus state) :chat) (submit state) (repl-submit state)))
        (t (let ((pane (focused state)))
             (multiple-value-bind (text cursor) (edit (pane-composer pane) (pane-cursor pane) key)
               (values (with-focused state (pane-update pane :composer text :cursor cursor)) nil)))))))

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
      (:review (values (commit (flush-tail state)
                               (lines :dim (format nil "review: ~(~A~)~@[, ~A~]" (second args)
                                                   (and (plusp (length (third args))) (third args)))))
                       nil))
      (:approve (if (eq (state-phase state) :approving)
                    (values state (list (list :resolve (second args) nil)))
                    (values (update state :phase :approving :approval (second args)) nil)))
      (:repl-result (values (repl-commit state (lines :output (second args))) nil))
      (:done (finish-turn state (first args)))
      (t (values state nil)))))

;;; Rendering: the whole screen every time, chat left, REPL right.

(defun status-text (state)
  (format nil "~A · ~(~A~) · ~(~A~)~@[ · ~D queued~] · tab: ~(~A~)"
          *model* (state-phase state) *permission*
          (and (state-queue state) (length (state-queue state)))
          (if (eq (state-focus state) :chat) :repl :chat)))

(defun render (out state &optional cols rows)
  "Paint STATE on OUT: two panes of log rows, an input row, a status row."
  (multiple-value-bind (size-cols size-rows) (terminal-size)
    (let* ((cols (or cols size-cols))
           (rows (or rows size-rows))
           (left (max 20 (1- (floor (* cols 3) 5))))
           (right (max 10 (- cols left 1)))
           (log-rows (max 1 (- rows 2)))
           (chat (state-chat state))
           (repl (state-repl state))
           (chat-lines (append (pane-log chat)
                               (when (plusp (length (state-tail state)))
                                 (list (cons (state-tail-style state) (state-tail state))))))
           (approving (eq (state-phase state) :approving))
           (chat-input (if approving "run? [y/n]" (format nil "> ~A" (pane-composer chat))))
           (repl-input (format nil "* ~A" (pane-composer repl)))
           (chat-focus (eq (state-focus state) :chat)))
      (format out "~C[?2026h~C[?25l" #\Esc #\Esc)
      (loop for row from 1 to log-rows
            for chat-row in (visible-rows chat-lines log-rows left (pane-scroll chat))
            for repl-row in (visible-rows (pane-log repl) log-rows right (pane-scroll repl))
            do (goto out row 1)
               (print-cell out chat-row left)
               (write-string "│" out)
               (print-cell out repl-row right))
      (goto out (1- rows) 1)
      (print-cell out (cons (if chat-focus :plain :dim) chat-input) left)
      (write-string "│" out)
      (print-cell out (cons (if chat-focus :dim :plain) repl-input) right)
      (goto out rows 1)
      (print-cell out (cons :dim (status-text state)) cols)
      (let* ((pane (focused state))
             (width (if chat-focus left right))
             (offset (if chat-focus 0 (1+ left)))
             (prefix (if (and chat-focus approving)
                         chat-input
                         (concatenate 'string "> " (subseq (pane-composer pane) 0 (pane-cursor pane))))))
        (goto out (1- rows) (+ offset 1 (min (display-width prefix) (1- width)))))
      (format out "~C[?25h~C[?2026l" #\Esc #\Esc)
      (finish-output out))))

;;; Effects.

(defvar *agent* nil "The thread running the current turn, if any.")

(defun start-turn (mailbox history prompt &key (stream #'stream-turn))
  "Run one turn on a fresh thread, posting its events and a final (:done history) to MAILBOX."
  (setf *cancel* nil)
  (labels ((emit (event) (sb-concurrency:send-message mailbox event))
           (ask (id)
             (let ((reply (sb-concurrency:make-mailbox)))
               (emit (list :approve id reply))
               (sb-concurrency:receive-message reply)))
           (approve (id code)
             (case *permission*
               (:yolo t)
               (:auto (multiple-value-bind (decision rationale) (review-call prompt code :stream stream)
                        (emit (list :review id decision rationale))
                        (or (eq decision :clear) (ask id))))
               (t (ask id)))))
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
      (:cancel (setf *cancel* t)
               (when (and *agent* (sb-thread:thread-alive-p *agent*))
                 (sb-thread:interrupt-thread *agent* (lambda () (when *cancel* (error 'cancelled)))))
               nil)
      (:eval (let ((code (first args)))
               (sb-thread:make-thread
                (lambda () (sb-concurrency:send-message mailbox (list :repl-result code (run-lisp code))))
                :name "repl"))
             nil)
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
          *permission* (cond ((null permission) *permission*)
                             ((string-equal permission "yolo") :yolo)
                             ((string-equal permission "auto") :auto)
                             (t :ask)))))

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
  (open-store)
  (load-definitions)
  (let ((mailbox (sb-concurrency:make-mailbox))
        (state (make-state)))
    (with-terminal (in out)
      (let ((reader (sb-thread:make-thread (lambda () (read-keys in mailbox)) :name "stdin")))
        (install-resize mailbox)
        (unwind-protect
             (block ui
               (render out state)
               (loop
                 (dolist (event (drain mailbox))
                   (multiple-value-bind (next effects) (reduce-event state event)
                     (setf state next)
                     (dolist (effect effects)
                       (when (eq (run-effect effect mailbox) :exit) (return-from ui)))))
                 (render out state)))
          (setf *cancel* t)
          (when (state-approval state) (sb-concurrency:send-message (state-approval state) nil))
          (when *agent*
            (sb-thread:join-thread *agent* :default nil :timeout 5)
            (when (sb-thread:thread-alive-p *agent*)
              (sb-thread:terminate-thread *agent*)
              (sb-thread:join-thread *agent* :default nil :timeout 1)))
          (sb-thread:terminate-thread reader)
          (sb-thread:join-thread reader :default nil :timeout 1))))
    (close-store)))

(defun save-image (path)
  "Save this image to PATH with MAIN as its toplevel; the build step. Does not return."
  (dex:clear-connection-pool)
  (setf *agent* nil *cancel* nil)
  (sb-ext:save-lisp-and-die path :toplevel #'main))
