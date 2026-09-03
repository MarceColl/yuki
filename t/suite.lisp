(defpackage #:yuuki/test
  (:use #:cl #:fiveam))

(in-package #:yuuki/test)

(def-suite :yuuki)
(in-suite :yuuki)

(defmacro with-temp-store (&body body)
  "Run BODY with a fresh definitions store in a temporary file."
  `(uiop:with-temporary-file (:pathname temp :type "sqlite3")
     (let ((yuuki::*store* nil))
       (yuuki::open-store temp)
       (unwind-protect (progn ,@body)
         (yuuki::close-store)))))
