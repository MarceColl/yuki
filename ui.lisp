(in-package #:yuuki)

;;; Keys.

(defun control-key (byte)
  (case byte
    ((13 10) :enter) ((127 8) :backspace) (3 :ctrl-c) (4 :ctrl-d)
    (1 :home) (5 :end) (21 :ctrl-u) (9 #\Tab)
    (t (when (>= byte 32) (code-char byte)))))

(defun csi-key (params final)
  (cond ((string= params "")
         (case final (#\A :up) (#\B :down) (#\C :right) (#\D :left)
               (#\H :home) (#\F :end)))
        ((char= final #\~)
         (cond ((string= params "3") :delete)
               ((string= params "1") :home)
               ((string= params "4") :end)))))

(defun ss3-key (byte)
  (case byte
    (65 :up) (66 :down) (67 :right) (68 :left) (72 :home) (70 :end)))

(defun utf8-length (lead)
  (cond ((>= lead #xF0) 4) ((>= lead #xE0) 3) (t 2)))

(defun utf8-decode (bytes)
  (sb-ext:octets-to-string
   (coerce bytes '(vector (unsigned-byte 8)))
   :external-format :utf-8))

(defun paste-end-p (reversed-bytes)
  "True when the newest six bytes are ESC [ 2 0 1 ~."
  (and (>= (length reversed-bytes) 6)
       (equal (subseq reversed-bytes 0 6) '(126 49 48 50 91 27))))

(defun decode-byte (state byte)
  "Pure key decoder step. STATE starts as (:ground). Returns state and keys."
  (destructuring-bind (mode &rest acc) state
    (case mode
      (:ground
       (cond ((= byte 27) (values '(:esc) nil))
             ((>= byte #xC0)
              (values (list :utf8 (utf8-length byte) (list byte)) nil))
             (t (values state (remove nil (list (control-key byte)))))))
      (:esc
       (case byte
         (91 (values '(:csi) nil))
         (79 (values '(:ss3) nil))
         ;; boffin: unknown escape prefixes return to normal byte decoding.
         (t (decode-byte '(:ground) byte))))
      (:ss3
       (values '(:ground) (remove nil (list (ss3-key byte)))))
      (:csi
       (if (<= 64 byte 126)
           (let ((params (coerce (reverse acc) 'string)))
             (if (and (string= params "200") (= byte 126))
                 (values '(:paste) nil)
                 ;; boffin: unknown CSI finals are consumed before returning to ground.
                 (values '(:ground)
                         (remove nil (list (csi-key params (code-char byte)))))))
           (values (list* :csi (code-char byte) acc) nil)))
      (:paste
       (let ((acc (cons byte acc)))
         (if (paste-end-p acc)
             (values '(:ground)
                     (list (list :paste
                                 (substitute #\Newline #\Return
                                             (utf8-decode (reverse (nthcdr 6 acc)))))))
             (values (cons :paste acc) nil))))
      (:utf8
       (destructuring-bind (need bytes) acc
         (let ((bytes (cons byte bytes)))
           (if (= (length bytes) need)
               (values '(:ground)
                       (list (char (utf8-decode (reverse bytes)) 0)))
               (values (list :utf8 need bytes) nil))))))))

;;; Composer.

(defun edit (text cursor key)
  "Pure composer step. Returns the new text and cursor."
  (flet ((insert (string)
           (values (concatenate 'string
                                (subseq text 0 cursor)
                                string
                                (subseq text cursor))
                   (+ cursor (length string)))))
    (cond ((characterp key) (insert (string key)))
          ((and (consp key) (eq (first key) :paste)) (insert (second key)))
          (t (case key
               (:backspace
                (if (plusp cursor)
                    (values (concatenate 'string
                                         (subseq text 0 (1- cursor))
                                         (subseq text cursor))
                            (1- cursor))
                    (values text cursor)))
               (:delete
                (if (< cursor (length text))
                    (values (concatenate 'string
                                         (subseq text 0 cursor)
                                         (subseq text (1+ cursor)))
                            cursor)
                    (values text cursor)))
               (:left (values text (max 0 (1- cursor))))
               (:right (values text (min (length text) (1+ cursor))))
               (:home (values text 0))
               (:end (values text (length text)))
               (:ctrl-u (values "" 0))
               (t (values text cursor)))))))

;;; Width.

(defun char-width (char)
  (cond ((< (char-code char) 32) 0)
        ((plusp (sb-unicode:combining-class char)) 0)
        ((member (sb-unicode:east-asian-width char) '(:w :f)) 2)
        (t 1)))

(defun display-width (string)
  "Return STRING's terminal column width."
  (loop for char across string sum (char-width char)))

(defun text-lines (text)
  (or (uiop:split-string text :separator '(#\Newline)) (list "")))

(defun rows (text cols)
  "Return the terminal rows occupied by TEXT at COLS columns."
  (loop for line in (text-lines text)
        sum (max 1 (ceiling (display-width line) cols))))

(defun cursor-position (text cols)
  "Return the row and column where the cursor lands after TEXT."
  (let* ((lines (text-lines text))
         (width (display-width (car (last lines))))
         (row (if (plusp width) (1- (ceiling width cols)) 0)))
    (values (+ (loop for line in (butlast lines)
                     sum (max 1 (ceiling (display-width line) cols)))
               row)
            (- width (* row cols)))))
