(in-package #:yuuki)

;;; State: immutable in use; every transition returns a copy.

(defstruct pane
  (log '())          ; (style . text) lines, oldest first
  (composer "")
  (cursor 0)
  (scroll 0))        ; rows scrolled up from the end; 0 follows the tail

(defstruct view
  name
  object
  (scroll 0)         ; rows from the top
  (cursor 0)         ; selected row, when on-select is set
  (on-select nil))   ; function of the selected row

(defstruct state
  (history '())
  (phase :idle)      ; :idle | :running | :accepting
  (queue '())        ; prompts typed while running
  (accepting nil)    ; (spec . reply mailbox) while accepting
  (chat (make-pane))
  (repl (make-pane))
  (views '())        ; panes opened with show, in order
  (focus :chat)      ; :chat | :repl | a view name
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
               (:accepting (setf (state-accepting copy) value))
               (:chat (setf (state-chat copy) value))
               (:repl (setf (state-repl copy) value))
               (:views (setf (state-views copy) value))
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
  "The focused chat or REPL pane, nil when a view has focus."
  (case (state-focus state)
    (:chat (state-chat state))
    (:repl (state-repl state))))

(defun with-focused (state pane)
  (if (eq (state-focus state) :chat) (update state :chat pane) (update state :repl pane)))

(defun focus-order (state)
  (list* :chat :repl (mapcar #'view-name (state-views state))))

(defun next-focus (state)
  (let* ((order (focus-order state))
         (position (or (position (state-focus state) order :test #'equal) 0)))
    (nth (mod (1+ position) (length order)) order)))

(defun put-view (views name object on-select)
  "VIEWS with NAME showing OBJECT, replacing an existing view of that name in place."
  (let ((new (make-view :name name :object object :on-select on-select)))
    (if (find name views :key #'view-name :test #'equal)
        (mapcar (lambda (view)
                  (if (equal (view-name view) name)
                      (progn (setf (view-scroll new) (view-scroll view)
                                   (view-cursor new) (min (view-cursor view) (max 0 (1- (length (selectable-rows object))))))
                             new)
                      view))
                views)
        (append views (list new)))))

(defun focused-view (state)
  (find (state-focus state) (state-views state) :key #'view-name :test #'equal))

(defun with-view (state view)
  (update state :views (mapcar (lambda (v) (if (equal (view-name v) (view-name view)) view v)) (state-views state))))

(defun move-cursor (state delta)
  (let* ((view (focused-view state))
         (rows (and view (selectable-rows (view-object view)))))
    (if (and view rows)
        (let ((copy (copy-view view)))
          (setf (view-cursor copy) (max 0 (min (1- (length rows)) (+ (view-cursor view) delta))))
          (with-view state copy))
        state)))

(defun select-row (state)
  (let* ((view (focused-view state))
         (rows (and view (selectable-rows (view-object view)))))
    (if (and view rows (view-on-select view))
        (values state (list (list :select (view-on-select view) (nth (view-cursor view) rows))))
        (values state nil))))

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

(defun scroll-focused (state direction)
  "Scroll the focused pane or view ten rows; DIRECTION 1 is up, -1 is down."
  (let ((pane (focused state)))
    (if pane
        (with-focused state (pane-update pane :scroll (max 0 (+ (pane-scroll pane) (* 10 direction)))))
        (update state :views
                (mapcar (lambda (view)
                          (if (equal (view-name view) (state-focus state))
                              (make-view :name (view-name view) :object (view-object view)
                                         :scroll (max 0 (- (view-scroll view) (* 10 direction))))
                              view))
                        (state-views state))))))

(defun accept-lines (spec)
  "What the chat shows when a question arrives."
  (let ((prompt (getf spec :prompt)))
    (case (getf spec :type)
      (:choice (append (when prompt (lines :user prompt))
                       (loop for option in (getf spec :options) for index from 1
                             collect (cons :plain (format nil "  ~D. ~A" index (one-line option))))))
      (:string (when prompt (lines :user prompt)))
      (t nil))))

(defun accept-input (state)
  "The chat input row while a question is pending."
  (let* ((spec (car (state-accepting state)))
         (prompt (or (getf spec :prompt) "?")))
    (case (getf spec :type)
      (:boolean (format nil "~A [y/n]" prompt))
      (:choice (format nil "~A [1-~D]: ~A" prompt (length (getf spec :options)) (pane-composer (state-chat state))))
      (:string (format nil "~A: ~A" prompt (pane-composer (state-chat state))))
      (t prompt))))

(defun answer (state value &rest effects)
  "Resolve the pending question with VALUE and go back to running."
  (values (update state :phase :running :accepting nil)
          (cons (list :resolve (cdr (state-accepting state)) value) effects)))

(defun reduce-accepting-key (state key)
  (let* ((spec (car (state-accepting state)))
         (type (getf spec :type))
         (options (getf spec :options)))
    (case key
      (:ctrl-c (answer state nil (list :cancel)))
      (:ctrl-d (answer state nil (list :exit)))
      (:tab (values (update state :focus (next-focus state)) nil))
      (t (case type
           (:boolean (case key
                       (#\y (answer state t))
                       (#\n (answer state nil))
                       (t (values state nil))))
           (:choice (let ((pane (state-chat state)))
                      (if (eq key :enter)
                          (let ((index (ignore-errors (parse-integer (pane-composer pane))))
                                (cleared (update state :chat (pane-update pane :composer "" :cursor 0))))
                            (if (and index (<= 1 index (length options)))
                                (answer cleared (nth (1- index) options))
                                (values cleared nil)))
                          (multiple-value-bind (text cursor) (edit (pane-composer pane) (pane-cursor pane) key)
                            (values (update state :chat (pane-update pane :composer text :cursor cursor)) nil)))))
           (:string (let ((pane (state-chat state)))
                      (if (eq key :enter)
                          (answer (commit (update state :chat (pane-update pane :composer "" :cursor 0))
                                          (lines :user (pane-composer pane)))
                                  (pane-composer pane))
                          (multiple-value-bind (text cursor) (edit (pane-composer pane) (pane-cursor pane) key)
                            (values (update state :chat (pane-update pane :composer text :cursor cursor)) nil)))))
           (t (values state nil)))))))

(defun reduce-key (state key)
  (cond ((and (eq (state-phase state) :accepting) (eq (state-focus state) :chat))
         (reduce-accepting-key state key))
        (t (case key
             (:ctrl-d (values state (list (list :exit))))
             (:ctrl-c (values state (list (list (if (eq (state-phase state) :idle) :exit :cancel)))))
             (:tab (values (update state :focus (next-focus state)) nil))
             (:page-up (values (scroll-focused state 1) nil))
             (:page-down (values (scroll-focused state -1) nil))
             (:enter (case (state-focus state)
                       (:chat (submit state))
                       (:repl (repl-submit state))
                       (t (select-row state))))
             (:up (if (focused-view state) (values (move-cursor state -1) nil) (values state nil)))
             (:down (if (focused-view state) (values (move-cursor state 1) nil) (values state nil)))
             (t (let ((pane (focused state)))
                  (if pane
                      (multiple-value-bind (text cursor) (edit (pane-composer pane) (pane-cursor pane) key)
                        (values (with-focused state (pane-update pane :composer text :cursor cursor)) nil))
                      (values state nil))))))))

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
      (:accept (if (eq (state-phase state) :accepting)
                   (values state (list (list :resolve (second args) nil)))
                   (values (update (commit (flush-tail state) (accept-lines (first args)))
                                   :phase :accepting :accepting (cons (first args) (second args)))
                           nil)))
      (:show (values (update state :views (put-view (state-views state) (first args) (second args) (third args))) nil))
      (:hide (let ((views (remove (first args) (state-views state) :key #'view-name :test #'equal)))
               (values (update state :views views
                               :focus (if (equal (state-focus state) (first args)) :chat (state-focus state)))
                       nil)))
      (:repl-result (values (repl-commit state (lines :output (second args))) nil))
      (:done (finish-turn state (first args)))
      (t (values state nil)))))

;;; Rendering: the whole screen every time, chat left, REPL right.

(defun status-text (state)
  (format nil "~A · ~(~A~) · ~(~A~)~@[ · ~D queued~] · tab: ~(~A~)"
          *model* (state-phase state) *permission*
          (and (state-queue state) (length (state-queue state)))
          (next-focus state)))

(defun view-rows (state view height width)
  "VIEW's presentation rows, the selected row highlighted and kept in sight when the view has focus."
  (let* ((object (view-object view))
         (lines (present-safely object width))
         (selectable (and (view-on-select view) (selectable-rows object)))
         (line (and selectable (row-line object (view-cursor view))))
         (lines (if (and line (equal (state-focus state) (view-name view)) (< line (length lines)))
                    (loop for entry in lines for index from 0
                          collect (if (= index line) (cons :selected (cdr entry)) entry))
                    lines))
         (scroll (cond ((null line) (view-scroll view))
                       ((< line (view-scroll view)) line)
                       ((>= line (+ (view-scroll view) height)) (max 0 (1+ (- line height))))
                       (t (view-scroll view)))))
    (rows-from-top lines height width scroll)))

(defun right-rows (state log-rows width)
  "The rows of the right column: every shown view, then the REPL."
  (let* ((views (state-views state))
         (count (length views))
         (view-rows (if (zerop count) 0 (max 0 (min (- log-rows 4) (floor (* log-rows 2) 3)))))
         (each (if (zerop count) 0 (floor view-rows count))))
    (append (loop for view in views
                  append (cons (cons :dim (format nil "── ~(~A~)" (view-name view)))
                               (view-rows state view (max 0 (1- each)) width)))
            (visible-rows (pane-log (state-repl state)) (- log-rows (* each count)) width
                          (pane-scroll (state-repl state))))))

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
           (accepting (eq (state-phase state) :accepting))
           (chat-input (if accepting (accept-input state) (format nil "> ~A" (pane-composer chat))))
           (repl-input (format nil "* ~A" (pane-composer repl)))
           (chat-focus (eq (state-focus state) :chat)))
      (format out "~C[?2026h~C[?25l" #\Esc #\Esc)
      (loop for row from 1 to log-rows
            for chat-row in (visible-rows chat-lines log-rows left (pane-scroll chat))
            for right-row in (right-rows state log-rows right)
            do (goto out row 1)
               (print-cell out chat-row left)
               (write-string "│" out)
               (print-cell out right-row right))
      (goto out (1- rows) 1)
      (print-cell out (cons (if chat-focus :plain :dim) chat-input) left)
      (write-string "│" out)
      (print-cell out (cons (if (eq (state-focus state) :repl) :plain :dim) repl-input) right)
      (goto out rows 1)
      (print-cell out (cons :dim (status-text state)) cols)
      (let* ((pane (or (focused state) chat))
             (width (if chat-focus left right))
             (offset (if chat-focus 0 (1+ left)))
             (prefix (cond ((and chat-focus accepting)
                            (if (member (getf (car (state-accepting state)) :type) '(:string :choice))
                                (let ((typed (subseq (pane-composer chat) 0 (pane-cursor chat))))
                                  (concatenate 'string
                                               (subseq chat-input 0 (- (length chat-input) (length (pane-composer chat))))
                                               typed))
                                chat-input))
                           (t (concatenate 'string "> " (subseq (pane-composer pane) 0 (pane-cursor pane)))))))
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
             (declare (ignore id))
             (let ((reply (sb-concurrency:make-mailbox)))
               (emit (list :accept (list :type :boolean :prompt "run?") reply))
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
      (:select (let ((handler (first args)) (row (second args)))
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case (funcall handler row)
                      (error (condition)
                        (sb-concurrency:send-message mailbox (list :error (format nil "select failed: ~A~%" condition))))))
                  :name "select"))
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
    (setf *ui* mailbox)
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
          (when (state-accepting state) (sb-concurrency:send-message (cdr (state-accepting state)) nil))
          (setf *ui* nil)
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
