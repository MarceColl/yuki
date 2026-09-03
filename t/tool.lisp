(in-package #:yuuki/test)
(in-suite :yuuki)

(test run-lisp-prints-values-and-output
  (let ((result (yuuki:run-lisp "(princ \"hello\") (+ 1 2) (values 1 2)")))
    (is (search "hello" result))
    (is (search "=> 3" result))
    (is (search (format nil "=> 1~%=> 2") result))))

(test run-lisp-definitions-persist-and-are-listed
  (yuuki:run-lisp "(defun twice (x) \"Double X.\" (* 2 x))")
  (is (search "=> 8" (yuuki:run-lisp "(twice 4)")))
  (let ((listing (yuuki:definitions)))
    (is (search "(twice x)" listing))
    (is (search "Double X." listing)))
  (is (search "(defun twice (x)" (yuuki:source 'yuuki-user::twice))))

(test run-lisp-reports-errors
  (let ((result (yuuki:run-lisp "(princ \"before\") (error \"boom\") (princ \"after\")")))
    (is (search "before" result))
    (is (search "error: boom" result))
    (is (not (search "after" result)))))

(test run-lisp-times-out
  (let ((result (yuuki:run-lisp "(loop)" :timeout 1)))
    (is (search "timed out" result))))

(test run-lisp-truncates
  (let* ((yuuki:*max-result-chars* 100)
         (result (yuuki:run-lisp "(princ (make-string 1000 :initial-element #\\x))")))
    (is (<= (length result) 100))
    (is (search "truncated" result))))

(test run-lisp-reads-in-agent-package
  (is (search "YUUKI-USER" (yuuki:run-lisp "(package-name *package*)"))))

(test source-records-macros-and-variables
  (with-temp-store
   (yuuki:run-lisp "(defmacro m1 (a &optional b) `(list ,a ,b)) (defvar *v* 3 \"a var\")")
  (is (search "(defmacro m1 (a &optional b)" (yuuki:source 'yuuki-user::m1)))
  (is (search "(defvar *v* 3 \"a var\")" (yuuki:source 'yuuki-user::*v*)))))

(test source-falls-back-to-retained-lambda
  (let ((*package* (find-package '#:yuuki-user)))
    (eval (read-from-string "(defun outside (x) \"Defined outside run-lisp.\" (1+ x))")))
  (is (search "(defun outside (x)" (yuuki:source 'yuuki-user::outside))))

(test source-of-unknown-name
  (is (search "no source" (yuuki:source 'yuuki-user::never-defined))))
