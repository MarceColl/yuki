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
         (t (decode-byte '(:ground) byte))))
      (:ss3
       (values '(:ground) (remove nil (list (ss3-key byte)))))
      (:csi
       (if (<= 64 byte 126)
           (let ((params (coerce (reverse acc) 'string)))
             (if (and (string= params "200") (= byte 126))
                 (values '(:paste) nil)
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
            (min (- width (* row cols)) (1- cols)))))

;;; Terminal.

(defun raw-termios ()
  "A termios for raw input: no echo, no line editing, no signals, 8-bit, byte at a time."
  (let ((termios (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-iflag termios)
          (logandc2 (sb-posix:termios-iflag termios)
                    (logior sb-posix:brkint sb-posix:icrnl sb-posix:inlcr sb-posix:igncr
                            sb-posix:istrip sb-posix:ixon sb-posix:ixoff))
          (sb-posix:termios-lflag termios)
          (logandc2 (sb-posix:termios-lflag termios)
                    (logior sb-posix:echo sb-posix:icanon sb-posix:isig sb-posix:iexten))
          (sb-posix:termios-cflag termios)
          (logior (sb-posix:termios-cflag termios) sb-posix:cs8))
    (setf (aref (sb-posix:termios-cc termios) sb-posix:vmin) 1
          (aref (sb-posix:termios-cc termios) sb-posix:vtime) 0)
    termios))

(defmacro with-terminal ((in out) &body body)
  "Run BODY with the tty in raw mode and bracketed paste on; IN is a byte stream on fd 0,
OUT a UTF-8 character stream on fd 1. Restores the terminal on any exit."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (sb-posix:tcgetattr 0))
           (,in (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8) :buffering :none))
           (,out (sb-sys:make-fd-stream 1 :output t :external-format :utf-8 :buffering :full)))
       (sb-posix:tcsetattr 0 sb-posix:tcsanow (raw-termios))
       (format ,out "~C[?2004h" #\Esc)
       (finish-output ,out)
       (unwind-protect (progn ,@body)
         (format ,out "~C[?2004l~C[0m~%" #\Esc #\Esc)
         (finish-output ,out)
         (sb-posix:tcsetattr 0 sb-posix:tcsanow ,saved)))))

(sb-alien:define-alien-type nil
    (sb-alien:struct winsize
                     (row sb-alien:unsigned-short) (col sb-alien:unsigned-short)
                     (x sb-alien:unsigned-short) (y sb-alien:unsigned-short)))

(defun terminal-columns ()
  "Columns of the controlling terminal, 80 when unknown."
  (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
    (let ((status (sb-alien:alien-funcall
                   (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                            (* (sb-alien:struct winsize))))
                   1 #+darwin #x40087468 #+linux #x5413 (sb-alien:addr ws))))
      (if (and (zerop status) (plusp (sb-alien:slot ws 'col))) (sb-alien:slot ws 'col) 80))))

(defun read-keys (in mailbox)
  "Decode bytes from IN forever, posting (:key k) to MAILBOX."
  (loop with state = '(:ground)
        for byte = (read-byte in nil)
        while byte
        do (multiple-value-bind (next keys) (decode-byte state byte)
             (setf state next)
             (dolist (key keys) (sb-concurrency:send-message mailbox (list :key key))))))

;;; Painting.

(defun sgr (style)
  (case style (:dim "2") (:user "1") (:code "36") (:output "33") (:error "31") (t "0")))

(defun print-line (out line &key (newline t))
  "Print a styled LINE to OUT, resetting SGR after its text."
  (format out "~C[~Am~A~C[0m~:[~;~%~]" #\Esc (sgr (car line)) (cdr line) #\Esc newline))

(defun erase (out row)
  "Move up ROW rows and clear from there to the end of the screen."
  (when (plusp row) (format out "~C[~DA" #\Esc row))
  (format out "~C~C[J" #\Return #\Esc))

(defun paint (out lines prompt prefix cols)
  "Print LINES, then PROMPT without a newline, and leave the cursor after PREFIX
(the part of PROMPT before the cursor). Returns the row the cursor is on within
the painted region."
  (dolist (line lines) (print-line out line))
  (print-line out (cons :plain prompt) :newline nil)
  (multiple-value-bind (row col) (cursor-position prefix cols)
    (let ((up (- (1- (rows prompt cols)) row)))
      (when (plusp up) (format out "~C[~DA" #\Esc up))
      (write-char #\Return out)
      (when (plusp col) (format out "~C[~DC" #\Esc col))
      (+ (loop for line in lines sum (rows (cdr line) cols)) row))))
