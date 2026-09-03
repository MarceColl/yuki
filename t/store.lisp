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

(test record-failure-does-not-abort-the-batch
  (with-temp-store
    (yuuki::close-store)
    (let ((yuuki::*store* (sqlite:connect ":memory:")))
      (sqlite:disconnect yuuki::*store*)
      (let ((result (yuuki:run-lisp "(defun sv-broken (x) x) (princ \"second ran\")")))
        (is (search "not recorded" result))
        (is (search "second ran" result))
        (is (fboundp 'yuuki-user::sv-broken))))))

(test rollback-restores-a-defvar-value
  (with-temp-store
    (yuuki:run-lisp "(defvar *sv-rv* 1 \"one\")")
    (yuuki:run-lisp "(defvar *sv-rv* 2 \"two\")")
    (yuuki:run-lisp "(setf *sv-rv* 999)")
    (yuuki:rollback 'yuuki-user::*sv-rv*)
    (is (search "=> 1" (yuuki:run-lisp "*sv-rv*")))))

(test rollback-prefix-is-literal
  (with-temp-store
    (yuuki:run-lisp "(defun sv-wc (x) x)")
    (yuuki:run-lisp "(defun sv-wc (x) (1+ x))")
    (signals error (yuuki:rollback 'yuuki-user::sv-wc "_"))
    (signals error (yuuki:rollback 'yuuki-user::sv-wc "%"))))

(test form-text-ignores-caller-printer-settings
  (let ((form '(defun sv-ft (x) (list 1 2 3 4 5 6))))
    (is (string= (yuuki::form-text form)
                 (let ((*print-length* 2) (*print-level* 1) (*print-base* 2))
                   (yuuki::form-text form))))))

(test nested-and-expanded-definitions-are-recorded
  (with-temp-store
    (yuuki:run-lisp "(progn (defun sv-n1 () 1) (let ((k 2)) (defun sv-n2 () k)))")
    (yuuki:run-lisp "(defmacro sv-def-twice (a b) `(progn (defun ,a () :a) (defun ,b () :b)))")
    (yuuki:run-lisp "(sv-def-twice sv-n3 sv-n4)")
    (dolist (name '(yuuki-user::sv-n1 yuuki-user::sv-n2 yuuki-user::sv-n3 yuuki-user::sv-n4 yuuki-user::sv-def-twice))
      (is (not (null (yuuki::stored-form name))) (format nil "~A recorded" name)))
    (is (search "=> SV-N5" (yuuki:run-lisp "(defun sv-n5 () 5)")))
    (is (not (search "defun" (yuuki:definitions))))))

(test methods-are-keyed-by-specializers
  (with-temp-store
    (yuuki:run-lisp "(defgeneric sv-size (x)) (defmethod sv-size ((x string)) (length x)) (defmethod sv-size ((x list)) (list-length x))")
    (let ((keys (sqlite:execute-to-list yuuki::*store* "SELECT name FROM bindings WHERE name LIKE 'SV-SIZE%' ORDER BY name")))
      (is (= 3 (length keys)))
      (is (find "SV-SIZE (LIST)" keys :key #'first :test #'string=))
      (is (find "SV-SIZE (STRING)" keys :key #'first :test #'string=)))
    (is (search "(x list)" (yuuki:history "SV-SIZE (LIST)")))))
