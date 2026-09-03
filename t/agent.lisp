(in-package #:yuuki/test)

(in-suite :yuuki)

(defun item-text (item)
  "Text of the first content part of a user or message item."
  (gethash "text" (aref (gethash "content" item) 0)))

(defun fake-stream (responses)
  "A stream-turn stand-in that pops one canned (items . finish) per call and records requests."
  (let ((seen '()))
    (values (lambda (instructions items emit &rest options)
              (declare (ignore instructions options))
              (push items seen)
              (destructuring-bind (items . finish) (pop responses)
                (dolist (item items)
                  (when (equal (gethash "type" item) "message")
                    (funcall emit (list :text (item-text item)))))
                (values items finish)))
            (lambda () (reverse seen)))))

(defun message-item (text)
  (yuuki::obj "type" "message" "role" "assistant" "status" "completed"
              "content" (vector (yuuki::obj "type" "output_text" "text" text))))

(defun call-item (id code)
  (yuuki::obj "type" "function_call" "call_id" id "name" "lisp"
              "arguments" (com.inuoe.jzon:stringify (yuuki::obj "code" code))))

(defun review-item (decision &optional (rationale "because"))
  (yuuki::obj "type" "function_call" "call_id" "r1" "name" "permission_decision"
              "arguments" (com.inuoe.jzon:stringify (yuuki::obj "decision" decision "rationale" rationale))))

(test review-call-parses-decisions
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (review-item "clear" "fine")) :stop)))
    (multiple-value-bind (decision rationale) (yuuki::review-call "req" "(+ 1 2)" :stream stream)
      (is (eq :clear decision))
      (is (string= "fine" rationale)))
    (let ((items (first (funcall seen))))
      (is (= 1 (length items)))
      (is (search "(+ 1 2)" (item-text (first items))))))
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (review-item "caution" "risky")) :stop)))
    (declare (ignore seen))
    (multiple-value-bind (decision rationale) (yuuki::review-call "req" "(delete-file \"x\")" :stream stream)
      (is (eq :caution decision))
      (is (string= "risky" rationale))))
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (message-item "no tool call")) :stop)))
    (declare (ignore seen))
    (is (eq :caution (yuuki::review-call "req" "1" :stream stream))))
  (is (eq :caution (yuuki::review-call "req" "1"
                                       :stream (lambda (&rest args) (declare (ignore args)) (error "down")))))
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (yuuki::obj "type" "function_call" "call_id" "x" "name" "other_tool"
                                                 "arguments" "{\"decision\":\"clear\",\"rationale\":\"\"}"))
                               :stop)))
    (declare (ignore seen))
    (is (eq :caution (yuuki::review-call "req" "1" :stream stream))))
  (signals yuuki::cancelled
    (yuuki::review-call "req" "1" :stream (lambda (&rest args) (declare (ignore args)) (error 'yuuki::cancelled)))))

(defun last-item-text (request)
  (item-text (car (last request))))

(test run-turn-appends-user-and-final-answer
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (message-item "hello")) :stop)))
    (let* ((events '())
           (history (yuuki::run-turn '() "hi"
                                     :emit (lambda (e) (push e events))
                                     :approve (lambda (id code) (declare (ignore id code)) t)
                                     :stream stream)))
      (is (= 2 (length history)))
      (is (string= "user" (gethash "role" (first history))))
      (is (string= "hi" (item-text (first history))))
      (is (string= "message" (gethash "type" (second history))))
      (is (string= "hello" (item-text (second history))))
      (is (null (gethash "id" (second history))))
      (let ((request (first (funcall seen))))
        (is (= 1 (length request)))
        (is (search "<task>" (last-item-text request)))
        (is (search "hi" (last-item-text request)))
        (is (search "<state>" (last-item-text request)))
        (is (search "first step" (last-item-text request))))
      (is (member '(:text "hello") events :test #'equal)))))

(test run-turn-feeds-the-observation-and-never-replays-steps
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)
                         (cons (list (message-item "done")) :stop)))
    (let* ((events '())
           (history (yuuki::run-turn '(:prior) "add"
                                     :emit (lambda (e) (push e events))
                                     :approve (lambda (id code) (declare (ignore id code)) t)
                                     :stream stream)))
      (is (= 3 (length history)))
      (is (eq :prior (first history)))
      (is (string= "done" (item-text (third history))))
      (is (= 2 (length (funcall seen))))
      (let ((second-request (second (funcall seen))))
        (is (= 2 (length second-request)))
        (is (eq :prior (first second-request)))
        (is (search "(+ 1 2)" (last-item-text second-request)))
        (is (search "=> 3" (last-item-text second-request)))
        (is (search "step 2; code already run this turn" (last-item-text second-request)))
        (is (search "result of the code you ran in the previous step" (last-item-text second-request)))
        (is (search "step 1" (last-item-text (first (funcall seen)))))
        (is (not (search "already run" (last-item-text (first (funcall seen))))))
        (is (notany (lambda (item) (and (hash-table-p item) (equal (gethash "type" item) "function_call")))
                    second-request)))
      (is (member '(:call "c1" "(+ 1 2)") events :test #'equal))
      (is (find :result events :key #'first)))))

(test run-turn-denied-call-is-observed
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (call-item "c1" "(delete-file \"x\")")) :stop)
                         (cons (list (message-item "ok")) :stop)))
    (yuuki::run-turn '() "rm"
                     :emit (lambda (e) (declare (ignore e)))
                     :approve (lambda (id code) (declare (ignore id code)) nil)
                     :stream stream)
    (is (search "denied by user" (last-item-text (second (funcall seen)))))))

(test run-turn-observes-two-calls-a-note-and-malformed-arguments
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (message-item "checking both")
                                     (call-item "c1" "(+ 1 2)")
                                     (yuuki::obj "type" "function_call" "call_id" "c2" "name" "lisp" "arguments" "{not json"))
                         :stop)
                         (cons (list (message-item "done")) :stop)))
    (let ((history (yuuki::run-turn '() "go"
                                    :emit (lambda (e) (declare (ignore e)))
                                    :approve (lambda (id code) (declare (ignore id code)) t)
                                    :stream stream)))
      (is (= 2 (length history)))
      (let ((observation (last-item-text (second (funcall seen)))))
        (is (search "your note: checking both" observation))
        (is (< (search "(+ 1 2)" observation) (search "=> 3" observation)))
        (is (< (search "=> 3" observation) (search "malformed arguments" observation)))))))

(test run-turn-empty-final-answer-appends-nothing
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (message-item "")) :stop)))
    (declare (ignore seen))
    (let ((history (yuuki::run-turn '() "hi"
                                    :emit (lambda (e) (declare (ignore e)))
                                    :approve (lambda (id code) (declare (ignore id code)) t)
                                    :stream stream)))
      (is (= 1 (length history))))))

(test run-turn-honours-cancel-before-running-calls
  (let ((yuuki::*cancel* t) (approved 0))
    (multiple-value-bind (stream seen)
        (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)))
      (let ((history (yuuki::run-turn '(:prior) "add"
                                      :emit (lambda (e) (declare (ignore e)))
                                      :approve (lambda (id code) (declare (ignore id code)) (incf approved) t)
                                      :stream stream)))
        (is (= 1 (length (funcall seen))))
        (is (= 0 approved))
        (is (equal '(:prior) history))))))

(test run-turn-stops-at-step-limit
  (let ((yuuki:*max-steps* 2) (events '()))
    (multiple-value-bind (stream seen)
        (fake-stream (list (cons (list (call-item "c1" "1")) :stop)
                           (cons (list (call-item "c2" "2")) :stop)
                           (cons (list (message-item "never")) :stop)))
      (let ((history (yuuki::run-turn '() "loop"
                                      :emit (lambda (e) (push e events))
                                      :approve (lambda (id code) (declare (ignore id code)) t)
                                      :stream stream)))
        (is (= 2 (length (funcall seen))))
        (is (= 1 (length history)))
        (is (find :error events :key #'first))))))

(test state-block-shows-values-and-is-bounded
  (yuuki:run-lisp "(defvar *findings* (list :a 1) \"What I know.\")")
  (let ((block (yuuki::state-block)))
    (is (search "*findings* = (:a 1)  ; What I know." block)))
  (let ((yuuki:*max-result-chars* 60))
    (is (<= (length (yuuki::state-block)) 60))))

(test instructions-mention-workspace-and-date
  (let ((text (yuuki::instructions)))
    (is (search "workspace:" text))
    (is (search "date:" text))
    (is (search "yuuki-user" text))))
