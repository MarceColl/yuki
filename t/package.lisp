(in-package #:yuuki/test)

(in-suite :yuuki)

(test obj-builds-equal-hash-table
  (let ((table (yuuki::obj "a" 1 "b" "two")))
    (is (eq (hash-table-test table) 'equal))
    (is (= 1 (gethash "a" table)))
    (is (string= "two" (gethash "b" table)))))

(test path-walks-and-tolerates-missing
  (let ((table (yuuki::obj "outer" (yuuki::obj "inner" 3))))
    (is (= 3 (yuuki::path table "outer" "inner")))
    (is (null (yuuki::path table "outer" "nope" "deeper")))
    (is (null (yuuki::path nil "x")))
    (is (null (yuuki::path (yuuki::obj "a" 7) "a" "b")))))
