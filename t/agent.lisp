(in-package #:yuuki/test)

(in-suite :yuuki)

(defun item-text (item)
  "Text of the first content part of a user or message item."
  (gethash "text" (aref (gethash "content" item) 0)))

(defun fake-stream (responses)
  "A stream-turn stand-in that pops one canned (items . finish) per call and records requests."
  (let ((seen '()))
    (values (lambda (instructions items emit)
              (declare (ignore instructions))
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

(test run-turn-appends-user-and-output
  (multiple-value-bind (stream seen) (fake-stream (list (cons (list (message-item "hello")) :stop)))
    (let* ((events '())
           (history (yuuki::run-turn '() "hi"
                                     :emit (lambda (e) (push e events))
                                     :approve (lambda (id) (declare (ignore id)) t)
                                     :stream stream)))
      (is (= 2 (length history)))
      (is (string= "user" (gethash "role" (first history))))
      (is (string= "hi" (item-text (first history))))
      (is (string= "message" (gethash "type" (second history))))
      (is (= 1 (length (funcall seen))))
      (is (member '(:text "hello") events :test #'equal)))))

(test run-turn-runs-approved-calls-and-loops
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)
                         (cons (list (message-item "done")) :stop)))
    (let* ((events '())
           (history (yuuki::run-turn '() "add"
                                     :emit (lambda (e) (push e events))
                                     :approve (lambda (id) (declare (ignore id)) t)
                                     :stream stream)))
      (is (= 4 (length history)))
      (is (string= "function_call_output" (gethash "type" (third history))))
      (is (string= "c1" (gethash "call_id" (third history))))
      (is (search "=> 3" (gethash "output" (third history))))
      (is (= 2 (length (funcall seen))))
      (is (member '(:call "c1" "(+ 1 2)") events :test #'equal))
      (is (find :result events :key #'first)))))

(test run-turn-denied-call
  (multiple-value-bind (stream seen)
      (fake-stream (list (cons (list (call-item "c1" "(delete-file \"x\")")) :stop)
                         (cons (list (message-item "ok")) :stop)))
    (declare (ignore seen))
    (let ((history (yuuki::run-turn '() "rm"
                                    :emit (lambda (e) (declare (ignore e)))
                                    :approve (lambda (id) (declare (ignore id)) nil)
                                    :stream stream)))
      (is (string= "denied by user" (gethash "output" (third history)))))))

(test run-turn-honours-cancel-before-running-calls
  (let ((yuuki::*cancel* t) (approved 0))
    (multiple-value-bind (stream seen)
        (fake-stream (list (cons (list (call-item "c1" "(+ 1 2)")) :stop)))
      (let ((history (yuuki::run-turn '() "add"
                                      :emit (lambda (e) (declare (ignore e)))
                                      :approve (lambda (id) (declare (ignore id)) (incf approved) t)
                                      :stream stream)))
        (is (= 1 (length (funcall seen))))
        (is (= 0 approved))
        (is (= 2 (length history)))))))

(test run-turn-stops-at-step-limit
  (let ((yuuki:*max-steps* 2))
    (multiple-value-bind (stream seen)
        (fake-stream (list (cons (list (call-item "c1" "1")) :stop)
                           (cons (list (call-item "c2" "2")) :stop)
                           (cons (list (message-item "never")) :stop)))
      (let ((history (yuuki::run-turn '() "loop"
                                      :emit (lambda (e) (declare (ignore e)))
                                      :approve (lambda (id) (declare (ignore id)) t)
                                      :stream stream)))
        (is (= 2 (length (funcall seen))))
        (is (search "Step limit" (item-text (car (last history)))))))))

(test instructions-mention-workspace-and-date
  (let ((text (yuuki::instructions)))
    (is (search "workspace:" text))
    (is (search "date:" text))
    (is (search "yuuki-user" text))))
