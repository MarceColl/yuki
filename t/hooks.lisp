(in-package #:yuuki/test)

(in-suite :yuuki)

(test hooks-run-in-registration-order-and-can-be-replaced
  (let ((yuuki::*hooks* (make-hash-table :test #'eq)))
    (let ((first (lambda (event) (declare (ignore event)) "first"))
          (replacement (lambda (event) (declare (ignore event)) '("second" "third"))))
      (yuuki::add-hook :turn-start first :name 'sample)
      (yuuki::add-hook :turn-start replacement :name 'sample)
      (is (= 1 (length (yuuki::list-hooks :turn-start))))
      (is (equal '("second" "third")
                 (yuuki::run-hooks :turn-start '(:prompt "hello") nil)))
      (is (yuuki::remove-hook :turn-start 'sample))
      (is (null (yuuki::list-hooks :turn-start)))
      (is (null (yuuki::remove-hook :turn-start 'sample))))))

(test hooks-receive-event-and-current-context
  (let ((yuuki::*hooks* (make-hash-table :test #'eq))
        (seen nil))
    (yuuki::add-hook :before-model
                     (lambda (event)
                       (setf seen event)
                       "new context"))
    (is (equal '("new context")
               (yuuki::run-hooks :before-model '(:step 2) '("old context"))))
    (is (eq :before-model (getf seen :event)))
    (is (= 2 (getf seen :step)))
    (is (equal '("old context") (getf seen :context)))))

(test hooks-reject-unknown-events-and-invalid-context
  (let ((yuuki::*hooks* (make-hash-table :test #'eq)))
    (signals error (yuuki::add-hook :unknown #'identity))
    (yuuki::add-hook :turn-start (lambda (event) (declare (ignore event)) 17))
    (signals type-error (yuuki::run-hooks :turn-start nil nil))))

(test instructions-include-hook-context
  (let ((text (yuuki::instructions '("first fact" "second fact"))))
    (is (search "<hook-context>" text))
    (is (search "first fact" text))
    (is (search "second fact" text))))
