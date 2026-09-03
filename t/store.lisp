(in-package #:yuuki/test)

(in-suite :yuuki)

(test record-binds-links-and-dedupes
  (with-temp-store
    (let ((first (yuuki::record '(defun sv-one (x) (* x 1))))
          (second (yuuki::record '(defun sv-one (x) (* x 2)))))
      (is (stringp first))
      (is (string/= first second))
      (is (string= second (yuuki::current-object 'yuuki-user::sv-one)))
      (is (= 2 (length (yuuki::versions 'yuuki-user::sv-one))))
      (is (string= second (first (first (yuuki::versions 'yuuki-user::sv-one)))))
      (is (equal '(defun sv-one (x) (* x 2)) (yuuki::stored-form 'yuuki-user::sv-one)))
      (is (string= second (yuuki::record '(defun sv-one (x) (* x 2)))))
      (is (= 2 (length (yuuki::versions 'yuuki-user::sv-one))))
      (is (string= first (yuuki::record '(defun sv-one (x) (* x 1)))))
      (is (= 2 (length (yuuki::versions 'yuuki-user::sv-one))))
      (is (null (yuuki::record '(+ 1 2))))
      (is (search "sv-one" (yuuki::history 'yuuki-user::sv-one)))
      (is (search "no versions" (yuuki::history 'yuuki-user::never))))))

(test run-lisp-records-and-rollback-restores
  (with-temp-store
    (yuuki:run-lisp "(defun sv-two (x) (* x 10))")
    (yuuki:run-lisp "(defun sv-two (x) (* x 20))")
    (is (search "=> 60" (yuuki:run-lisp "(sv-two 3)")))
    (is (search "(* x 20)" (yuuki:source 'yuuki-user::sv-two)))
    (yuuki:rollback 'yuuki-user::sv-two)
    (is (search "=> 30" (yuuki:run-lisp "(sv-two 3)")))
    (is (search "(* x 10)" (yuuki:source 'yuuki-user::sv-two)))
    (is (= 2 (length (yuuki::versions 'yuuki-user::sv-two))))
    (is (equal '(nil t) (mapcar #'fourth (yuuki::versions 'yuuki-user::sv-two))))
    (is (search "* " (yuuki:history 'yuuki-user::sv-two)))
    (signals error (yuuki:rollback 'yuuki-user::sv-two))
    (let ((twenty (find "(* x 20)" (yuuki::versions 'yuuki-user::sv-two) :key #'third :test #'search)))
      (yuuki:rollback 'yuuki-user::sv-two (subseq (first twenty) 0 8))
      (is (search "=> 60" (yuuki:run-lisp "(sv-two 3)")))
      (is (= 2 (length (yuuki::versions 'yuuki-user::sv-two)))))))

(test load-definitions-restores-macros-first
  (with-temp-store
    (yuuki:run-lisp "(defun sv-user () (sv-mac 41))")
    (yuuki:run-lisp "(defmacro sv-mac (x) `(1+ ,x))")
    (yuuki:run-lisp "(defvar *sv-var* 7 \"seven\")")
    (fmakunbound 'yuuki-user::sv-user)
    (fmakunbound 'yuuki-user::sv-mac)
    (makunbound 'yuuki-user::*sv-var*)
    (is (= 3 (yuuki::load-definitions)))
    (is (search "=> 42" (yuuki:run-lisp "(sv-user)")))
    (is (search "=> 7" (yuuki:run-lisp "*sv-var*")))))

(test record-is-a-no-op-without-a-store
  (let ((yuuki::*store* nil))
    (is (null (yuuki::record '(defun sv-none (x) x))))
    (is (null (yuuki::stored-form 'yuuki-user::sv-none)))))
