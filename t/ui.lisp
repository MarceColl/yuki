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

(test print-line-styles-and-resets
  (let ((text (with-output-to-string (out) (yuuki::print-line out '(:code . "x")))))
    (is (string= (format nil "~C[36mx~C[0m~%" #\Esc #\Esc) text))))

(test paint-returns-cursor-row-and-places-cursor
  (let* ((row nil)
         (text (with-output-to-string (out)
                 (setf row (yuuki::paint out '((:dim . "status")) "> hello" "> he" 80)))))
    (is (= 1 row))
    (is (search "status" text))
    (is (search "> hello" text))
    (is (search (format nil "~C[4C" #\Esc) text))))

(test paint-counts-wrapped-lines
  (let* ((row nil))
    (with-output-to-string (out)
      (setf row (yuuki::paint out '((:dim . "0123456789abcdef")) "> ab" "> ab" 10)))
    (is (= 2 row))))

(test erase-moves-up-and-clears
  (is (string= (format nil "~C[3A~C~C[J" #\Esc #\Return #\Esc)
               (with-output-to-string (out) (yuuki::erase out 3))))
  (is (string= (format nil "~C~C[J" #\Return #\Esc)
               (with-output-to-string (out) (yuuki::erase out 0)))))
