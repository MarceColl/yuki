(in-package #:yuuki/test)

(in-suite :yuuki)

(defun reduce-all (state events)
  "Reduce EVENTS in order; return (values state all-effects)."
  (let ((effects '()))
    (dolist (event events (values state (nreverse effects)))
      (multiple-value-bind (next produced) (yuuki::reduce-event state event)
        (setf state next)
        (dolist (effect produced) (push effect effects))))))

(test typing-edits-composer
  (let ((state (reduce-all (yuuki::make-state) '((:key #\h) (:key #\i) (:key :left)))))
    (is (string= "hi" (yuuki::state-composer state)))
    (is (= 1 (yuuki::state-cursor state)))))

(test enter-when-idle-starts-turn-and-echoes
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state :history '(:h)) '((:key #\h) (:key #\i) (:key :enter)))
    (is (eq :running (yuuki::state-phase state)))
    (is (string= "" (yuuki::state-composer state)))
    (is (equal '((:start (:h) "hi")) effects))
    (is (equal '((:user . "hi")) (yuuki::state-committed state)))))

(test enter-when-running-queues
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state :phase :running) '((:key #\x) (:key :enter)))
    (is (equal '("x") (yuuki::state-queue state)))
    (is (null effects))))

(test empty-enter-does-nothing
  (multiple-value-bind (state effects) (reduce-all (yuuki::make-state) '((:key :enter)))
    (is (eq :idle (yuuki::state-phase state)))
    (is (null effects))))

(test slash-line-is-evaluated-by-human
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state) '((:key #\/) (:key #\() (:key #\)) (:key :enter)))
    (is (eq :idle (yuuki::state-phase state)))
    (is (equal '((:eval "()")) effects))))

(test text-splits-into-committed-and-tail
  (let ((state (reduce-all (yuuki::make-state) (list (list :text (format nil "a~%b")) '(:text "c")))))
    (is (equal '((:plain . "a")) (yuuki::state-committed state)))
    (is (string= "bc" (yuuki::state-tail state)))))

(test reasoning-then-text-flushes-tail
  (let ((state (reduce-all (yuuki::make-state) '((:reasoning "think") (:text "say")))))
    (is (equal '((:dim . "think")) (yuuki::state-committed state)))
    (is (string= "say" (yuuki::state-tail state)))
    (is (eq :plain (yuuki::state-tail-style state)))))

(test call-and-result-commit-blocks
  (let ((state (reduce-all (yuuki::make-state)
                           (list '(:text "x")
                                 (list :call "c1" (format nil "(a)~%(b)"))
                                 '(:result "c1" "=> 1")))))
    (is (equal '((:plain . "x") (:code . "(a)") (:code . "(b)") (:output . "=> 1"))
               (yuuki::state-committed state)))
    (is (string= "" (yuuki::state-tail state)))))

(test approval-flow
  (let ((promise (sb-concurrency:make-mailbox)))
    (multiple-value-bind (state effects)
        (reduce-all (yuuki::make-state :phase :running)
                    (list (list :approve "c1" promise) '(:key #\y)))
      (is (eq :running (yuuki::state-phase state)))
      (is (equal (list (list :resolve promise t)) effects)))
    (multiple-value-bind (state effects)
        (reduce-all (yuuki::make-state :phase :running)
                    (list (list :approve "c1" promise) '(:key #\n)))
      (is (eq :running (yuuki::state-phase state)))
      (is (equal (list (list :resolve promise nil)) effects)))
    (multiple-value-bind (state effects)
        (reduce-all (yuuki::make-state :phase :running)
                    (list (list :approve "c1" promise) '(:key #\q)))
      (is (eq :approving (yuuki::state-phase state)))
      (is (null effects)))))

(test second-approval-while-approving-is-rejected
  (let ((first (sb-concurrency:make-mailbox))
        (second (sb-concurrency:make-mailbox)))
    (multiple-value-bind (state effects)
        (reduce-all (yuuki::make-state :phase :running)
                    (list (list :approve "c1" first) (list :approve "c2" second)))
      (is (eq :approving (yuuki::state-phase state)))
      (is (eq first (yuuki::state-approval state)))
      (is (equal (list (list :resolve second nil)) effects)))))

(test done-takes-history-and-starts-queued
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state :phase :running :queue '("next") :tail "end")
                  '((:done (:new))))
    (is (eq :running (yuuki::state-phase state)))
    (is (equal '(:new) (yuuki::state-history state)))
    (is (null (yuuki::state-queue state)))
    (is (equal '((:start (:new) "next")) effects))
    (is (equal '((:plain . "end") (:plain . "")) (yuuki::state-committed state)))))

(test done-with-empty-queue-goes-idle
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state :phase :running) '((:done (:new))))
    (is (eq :idle (yuuki::state-phase state)))
    (is (null effects))))

(test ctrl-c-cancels-or-exits
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state :phase :running) '((:key :ctrl-c)))
    (declare (ignore state))
    (is (equal '((:cancel)) effects)))
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state) '((:key :ctrl-c)))
    (declare (ignore state))
    (is (equal '((:exit)) effects)))
  (multiple-value-bind (state effects)
      (reduce-all (yuuki::make-state) '((:key :ctrl-d)))
    (declare (ignore state))
    (is (equal '((:exit)) effects))))

(test render-prints-committed-and-clears-them
  (let* ((state (yuuki::make-state :committed '((:user . "hi")) :tail "partial" :live-row 0))
         (out (make-string-output-stream))
         (next (yuuki::render out state)))
    (let ((text (get-output-stream-string out)))
      (is (search "hi" text))
      (is (search "partial" text))
      (is (search "> " text)))
    (is (null (yuuki::state-committed next)))
    (is (= 2 (yuuki::state-live-row next)))))

(test configure-reads-environment
  (let ((yuuki:*model* "from-image") (yuuki:*permission* :ask))
    (sb-posix:setenv "YUUKI_PERMISSION" "yolo" 1)
    (sb-posix:unsetenv "YUUKI_MODEL")
    (unwind-protect
         (progn (yuuki::configure)
                (is (eq :yolo yuuki:*permission*))
                (is (string= "from-image" yuuki:*model*)))
      (sb-posix:unsetenv "YUUKI_PERMISSION"))))

(test start-turn-posts-events-and-done
  (let ((mailbox (sb-concurrency:make-mailbox))
        (saved yuuki:*permission*))
    (setf yuuki:*permission* :yolo)
    (unwind-protect
         (multiple-value-bind (stream seen)
             (fake-stream (list (cons (list (message-item "hey")) :stop)))
           (declare (ignore seen))
           (yuuki::start-turn mailbox '() "hi" :stream stream)
           (sb-thread:join-thread yuuki::*agent*)
           (let ((events (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
                               while ok collect event)))
             (is (equal '(:text "hey") (first events)))
             (is (eq :done (first (car (last events)))))
             (is (= 2 (length (second (car (last events))))))))
      (setf yuuki:*permission* saved))))

(test start-turn-yolo-runs-calls-without-asking
  (let ((mailbox (sb-concurrency:make-mailbox))
        (saved yuuki:*permission*))
    (setf yuuki:*permission* :yolo)
    (unwind-protect
         (multiple-value-bind (stream seen)
             (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)
                                (cons (list (message-item "ok")) :stop)))
           (declare (ignore seen))
           (yuuki::start-turn mailbox '() "add" :stream stream)
           (sb-thread:join-thread yuuki::*agent*)
           (let ((events (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
                               while ok collect event)))
             (is (null (find :approve events :key #'first)))
             (is (search "=> 3" (third (find :result events :key #'first))))
             (is (= 4 (length (second (find :done events :key #'first)))))))
      (setf yuuki:*permission* saved))))

(test start-turn-auto-clear-runs-without-asking
  (let ((mailbox (sb-concurrency:make-mailbox))
        (saved yuuki:*permission*))
    (setf yuuki:*permission* :auto)
    (unwind-protect
         (multiple-value-bind (stream seen)
             (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)
                                (cons (list (review-item "clear" "fine")) :stop)
                                (cons (list (message-item "ok")) :stop)))
           (declare (ignore seen))
           (yuuki::start-turn mailbox '() "add" :stream stream)
           (sb-thread:join-thread yuuki::*agent*)
           (let ((events (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
                               while ok collect event)))
             (is (null (find :approve events :key #'first)))
             (is (equal '(:review "c1" :clear "fine") (find :review events :key #'first)))
             (is (search "=> 3" (third (find :result events :key #'first))))))
      (setf yuuki:*permission* saved))))

(test start-turn-auto-caution-asks
  (let ((mailbox (sb-concurrency:make-mailbox))
        (saved yuuki:*permission*))
    (setf yuuki:*permission* :auto)
    (unwind-protect
         (multiple-value-bind (stream seen)
             (fake-stream (list (cons (list (call-item "c1" "(delete-file \"x\")")) :stop)
                                (cons (list (review-item "caution" "risky")) :stop)
                                (cons (list (message-item "ok")) :stop)))
           (declare (ignore seen))
           (yuuki::start-turn mailbox '() "rm" :stream stream)
           (let ((approve (loop for event = (sb-concurrency:receive-message mailbox :timeout 5)
                                until (or (null event) (eq (first event) :approve))
                                finally (return event))))
             (is (equal "c1" (second approve)))
             (sb-concurrency:send-message (third approve) nil)
             (sb-thread:join-thread yuuki::*agent*)
             (let ((events (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
                                 while ok collect event)))
               (is (equal "denied by user" (third (find :result events :key #'first)))))))
      (setf yuuki:*permission* saved))))

(test review-event-commits-a-dim-line
  (let ((state (reduce-all (yuuki::make-state) '((:review "c1" :caution "risky") (:review "c2" :clear "")))))
    (is (equal '((:dim . "review: caution, risky") (:dim . "review: clear")) (yuuki::state-committed state)))))

(test configure-accepts-auto
  (let ((yuuki:*permission* :ask))
    (sb-posix:setenv "YUUKI_PERMISSION" "auto" 1)
    (unwind-protect (progn (yuuki::configure) (is (eq :auto yuuki:*permission*)))
      (sb-posix:unsetenv "YUUKI_PERMISSION"))))

(test start-turn-ask-posts-approval-and-waits
  (let ((mailbox (sb-concurrency:make-mailbox))
        (saved yuuki:*permission*))
    (setf yuuki:*permission* :ask)
    (unwind-protect
         (multiple-value-bind (stream seen)
             (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)
                                (cons (list (message-item "ok")) :stop)))
           (declare (ignore seen))
           (yuuki::start-turn mailbox '() "add" :stream stream)
           (let ((approve (loop for event = (sb-concurrency:receive-message mailbox :timeout 5)
                                until (or (null event) (eq (first event) :approve))
                                finally (return event))))
             (is (equal "c1" (second approve)))
             (sb-concurrency:send-message (third approve) nil)
             (sb-thread:join-thread yuuki::*agent*)
             (let ((events (loop for (event ok) = (multiple-value-list (sb-concurrency:receive-message-no-hang mailbox))
                                 while ok collect event)))
               (is (equal "denied by user" (third (find :result events :key #'first))))
               (is (find :done events :key #'first)))))
      (setf yuuki:*permission* saved))))
