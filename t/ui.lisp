(in-package #:yuuki/test)

(in-suite :yuuki)

(defun decode-all (bytes)
  "Feed BYTES through decode-byte from the ground state; return the keys in order."
  (let ((state '(:ground)) (keys '()))
    (dolist (byte bytes (nreverse keys))
      (multiple-value-bind (next produced) (yuuki::decode-byte state byte)
        (setf state next)
        (dolist (key produced) (push key keys))))))

(defun bytes (string)
  (coerce (sb-ext:string-to-octets string :external-format :utf-8) 'list))

(test decode-printable-and-controls
  (is (equal '(#\a #\b) (decode-all (bytes "ab"))))
  (is (equal '(:enter) (decode-all '(13))))
  (is (equal '(:backspace) (decode-all '(127))))
  (is (equal '(:ctrl-c :ctrl-d :ctrl-u) (decode-all '(3 4 21))))
  (is (equal '(:home :end) (decode-all '(1 5)))))

(test decode-escape-sequences
  (is (equal '(:up :down :right :left)
             (decode-all (list 27 91 65 27 91 66 27 91 67 27 91 68))))
  (is (equal '(:up) (decode-all (list 27 91 65))))
  (is (equal '(:right) (decode-all (list 27 79 67))))
  (is (equal '(:delete) (decode-all (list 27 91 51 126))))
  (is (equal '(:home :end) (decode-all (list 27 91 72 27 91 70)))))

(test decode-lone-escape-does-not-swallow-next-key
  (is (equal '(#\a) (decode-all (list 27 97)))))

(test decode-unknown-csi-final-returns-to-ground
  (is (equal '(#\a) (decode-all (list 27 91 49 122 97)))))

(test decode-unknown-ss3-final-returns-to-ground
  (is (equal '(#\a) (decode-all (list 27 79 90 97)))))

(test decode-utf8
  (is (equal (list (code-char 233)) (decode-all (bytes "é"))))
  (is (equal (list (code-char #x65E5)) (decode-all (bytes "日")))))

(test decode-bracketed-paste
  (let ((keys (decode-all
               (append (list 27 91 50 48 48 126)
                       (bytes "hi") (list 13) (bytes "x")
                       (list 27 91 50 48 49 126)))))
    (is (equal (list (list :paste (format nil "hi~%x"))) keys))))

(test decode-bracketed-paste-preserves-escape-content
  (let ((keys (decode-all
               (append (list 27 91 50 48 48 126)
                       (bytes "a") (list 27 91 120)
                       (list 27 91 50 48 49 126)))))
    (is (equal (list (list :paste (format nil "a~C[x" #\Esc))) keys))))

(test edit-inserts-deletes-moves
  (multiple-value-bind (text cursor) (yuuki::edit "ac" 1 #\b)
    (is (string= "abc" text)) (is (= 2 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 3 :backspace)
    (is (string= "ab" text)) (is (= 2 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 0 :backspace)
    (is (string= "abc" text)) (is (= 0 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 1 :delete)
    (is (string= "ac" text)) (is (= 1 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 1 :left)
    (is (string= "abc" text)) (is (= 0 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 0 :left)
    (is (= 0 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 3 :right)
    (is (= 3 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 1 :end)
    (is (= 3 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 1 '(:paste "XY"))
    (is (string= "aXYbc" text)) (is (= 3 cursor)))
  (multiple-value-bind (text cursor) (yuuki::edit "abc" 1 :ctrl-u)
    (is (string= "" text)) (is (= 0 cursor))))

(test display-width-handles-wide-and-combining
  (is (= 3 (yuuki::display-width "abc")))
  (is (= 4 (yuuki::display-width "日本")))
  (is (= 1 (yuuki::display-width
            (coerce (list #\e (code-char #x301)) 'string)))))

(test rows-and-cursor-position
  (is (= 1 (yuuki::rows "" 10)))
  (is (= 1 (yuuki::rows "abcdefghij" 10)))
  (is (= 2 (yuuki::rows "abcdefghijk" 10)))
  (is (= 2 (yuuki::rows (format nil "a~%b") 10)))
  (multiple-value-bind (row col) (yuuki::cursor-position "abc" 10)
    (is (= 0 row)) (is (= 3 col)))
  (multiple-value-bind (row col) (yuuki::cursor-position "abcdefghij" 10)
    (is (= 0 row)) (is (= 9 col)))
  (multiple-value-bind (row col) (yuuki::cursor-position "abcdefghijkl" 10)
    (is (= 1 row)) (is (= 2 col)))
  (multiple-value-bind (row col) (yuuki::cursor-position (format nil "ab~%c") 10)
    (is (= 1 row)) (is (= 1 col))))

(test print-cell-pads-and-resets
  (let ((text (with-output-to-string (out) (yuuki::print-cell out '(:code . "x") 4))))
    (is (string= (format nil "~C[36mx   ~C[0m" #\Esc #\Esc) text))))

(test wrap-pad-and-visible-rows
  (is (equal '("abcd" "ef") (yuuki::wrap-line "abcdef" 4)))
  (is (equal '("") (yuuki::wrap-line "" 4)))
  (is (equal '("日本" "語") (yuuki::wrap-line "日本語" 4)))
  (is (string= "ab  " (yuuki::pad "ab" 4)))
  (is (string= "abcd" (yuuki::pad "abcdef" 4)))
  (is (string= "日 " (yuuki::pad "日本" 3)))
  (let ((lines '((:user . "one") (:plain . "twotwo") (:plain . "three"))))
    (is (equal '((:plain . "thr") (:plain . "ee")) (yuuki::visible-rows lines 2 3 0)))
    (is (equal '((:plain . "two") (:plain . "thr")) (yuuki::visible-rows lines 2 3 1)))
    (is (equal '((:plain . "") (:user . "one") (:plain . "twotwo") (:plain . "three"))
               (yuuki::visible-rows lines 4 6 0)))
    (is (equal '((:user . "one") (:plain . "twotwo") (:plain . "three")) (yuuki::visible-rows lines 3 6 99)))))

(test decode-tab-and-paging
  (is (equal '(:tab) (decode-all '(9))))
  (is (equal '(:page-up :page-down) (decode-all (list 27 91 53 126 27 91 54 126)))))

(test present-built-ins
  (is (equal '((:plain . "a") (:plain . "b")) (yuuki:present (format nil "a~%b") 40)))
  (is (equal '((:dim . "nil")) (yuuki:present nil 40)))
  (let ((table (yuuki:present '(("name" "bytes") ("app.lisp" 14897) ("x" 1)) 40)))
    (is (eq :user (car (first table))))
    (is (string= "name      bytes" (cdr (first table))))
    (is (string= "app.lisp  14897" (cdr (second table))))
    (is (string= "x         1" (cdr (third table)))))
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "k" h) '(1 2))
    (is (equal '((:plain . "k: (1 2)")) (yuuki:present h 40))))
  (is (equal '((:plain . "42")) (yuuki:present 42 40)))
  (is (equal '((:plain . "1") (:plain . "x")) (yuuki:present '(1 "x") 40)))
  (defmethod yuuki:present ((object (eql :boom)) width) (declare (ignore width)) (error "boom"))
  (is (search "present failed: boom" (cdr (first (yuuki::present-safely :boom 5)))))
  (is (equal '((:user . "h") (:plain . "b") (:plain . ""))
             (yuuki::rows-from-top '((:user . "h") (:plain . "b")) 3 10 0)))
  (is (equal '((:plain . "b") (:plain . "") (:plain . ""))
             (yuuki::rows-from-top '((:user . "h") (:plain . "b")) 3 10 1))))

(test present-is-extensible-by-method
  (yuuki:run-lisp "(defstruct sv-point x y) (defmethod present ((p sv-point) width) (list (cons :plain (format nil \"(~a, ~a)\" (sv-point-x p) (sv-point-y p)))))")
  (is (equal '((:plain . "(1, 2)"))
             (yuuki:present (let ((*package* (find-package '#:yuuki-user)))
                              (eval (read-from-string "(make-sv-point :x 1 :y 2)")))
                            40)))
  (is (search "(1, 2)" (yuuki:run-lisp "(present (make-sv-point :x 1 :y 2) 10)"))))

(test show-and-accept-post-to-the-interface
  (let ((saved yuuki::*ui*))
    (setf yuuki::*ui* (sb-concurrency:make-mailbox))
    (unwind-protect
    (progn
    (is (equal '(1 2) (yuuki:show "x" '(1 2))))
    (is (equal '(:show "x" (1 2)) (sb-concurrency:receive-message yuuki::*ui* :timeout 1)))
    (yuuki:hide "x")
    (is (equal '(:hide "x") (sb-concurrency:receive-message yuuki::*ui* :timeout 1)))
    (let ((asker (sb-thread:make-thread (lambda () (yuuki:accept :choice :prompt "q" :options '("a" "b"))))))
      (let ((event (sb-concurrency:receive-message yuuki::*ui* :timeout 2)))
        (is (eq :accept (first event)))
        (is (equal '("a" "b") (getf (second event) :options)))
        (sb-concurrency:send-message (third event) "b"))
      (is (equal "b" (sb-thread:join-thread asker :timeout 2)))))
      (setf yuuki::*ui* saved))))
