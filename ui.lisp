(in-package #:yuuki)

;;; Keys.

(defun control-key (byte)
  (case byte
    ((13 10) :enter) ((127 8) :backspace) (3 :ctrl-c) (4 :ctrl-d)
    (1 :home) (5 :end) (21 :ctrl-u) (9 :tab)
    (t (when (>= byte 32) (code-char byte)))))

(defun csi-key (params final)
  (cond ((string= params "")
         (case final (#\A :up) (#\B :down) (#\C :right) (#\D :left)
               (#\H :home) (#\F :end)))
        ((char= final #\~)
         (cond ((string= params "3") :delete)
               ((string= params "1") :home)
               ((string= params "4") :end)
               ((string= params "5") :page-up)
               ((string= params "6") :page-down)))))

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

;;; Cells.

(defun prefix-by-width (text width)
  "The longest prefix of TEXT that fits in WIDTH columns."
  (loop with used = 0
        for index from 0 below (length text)
        for char-width = (char-width (char text index))
        while (<= (+ used char-width) width)
        do (incf used char-width)
        finally (return (subseq text 0 index))))

(defun wrap-line (text width)
  "TEXT cut into pieces of at most WIDTH columns; at least one piece."
  (if (<= (display-width text) width)
      (list text)
      (let ((head (prefix-by-width text width)))
        (if (zerop (length head))
            (list (subseq text 0 1))
            (cons head (wrap-line (subseq text (length head)) width))))))

(defun pad (text width)
  "TEXT cut or space-padded to exactly WIDTH columns."
  (let ((fitted (prefix-by-width text width)))
    (concatenate 'string fitted (make-string (- width (display-width fitted)) :initial-element #\Space))))

(defun rows-from-top (lines count width scroll)
  "The COUNT (style . text) rows of LINES wrapped at WIDTH, starting SCROLL rows from the top."
  (let* ((rows (loop for (style . text) in lines
                     append (mapcar (lambda (piece) (cons style piece)) (wrap-line text width))))
         (start (min scroll (max 0 (- (length rows) 1))))
         (shown (subseq rows start (min (length rows) (+ start count)))))
    (append shown (make-list (- count (length shown)) :initial-element (cons :plain "")))))

(defun visible-rows (lines count width scroll)
  "The COUNT (style . text) rows of LINES wrapped at WIDTH, ending SCROLL rows before the last."
  (let* ((rows (loop for (style . text) in lines
                     append (mapcar (lambda (piece) (cons style piece)) (wrap-line text width))))
         (total (length rows))
         (end (max (min count total) (- total scroll)))
         (shown (subseq rows (max 0 (- end count)) end)))
    (append (make-list (- count (length shown)) :initial-element (cons :plain "")) shown)))

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
       (unwind-protect
            (progn
              (sb-posix:tcsetattr 0 sb-posix:tcsanow (raw-termios))
              (format ,out "~C[?1049h~C[?2004h" #\Esc #\Esc)
              (finish-output ,out)
              ,@body)
         (format ,out "~C[?2004l~C[0m~C[?25h~C[?1049l" #\Esc #\Esc #\Esc #\Esc)
         (finish-output ,out)
         (sb-posix:tcsetattr 0 sb-posix:tcsanow ,saved)))))

(sb-alien:define-alien-type nil
    (sb-alien:struct winsize
                     (row sb-alien:unsigned-short) (col sb-alien:unsigned-short)
                     (x sb-alien:unsigned-short) (y sb-alien:unsigned-short)))

(defun terminal-size ()
  "Columns and rows of the controlling terminal, 80 by 24 when unknown.
ioctl is variadic; &optional marks the pointer as a vararg, which matters on
arm64 Darwin where varargs travel on the stack. Without it the kernel writes
the winsize struct through a garbage pointer into the Lisp heap."
  (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
    (let ((status (sb-alien:alien-funcall
                   (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                            &optional (* (sb-alien:struct winsize))))
                   1 #+darwin #x40087468 #+linux #x5413 (sb-alien:addr ws))))
      (if (and (zerop status) (plusp (sb-alien:slot ws 'col)) (plusp (sb-alien:slot ws 'row)))
          (values (sb-alien:slot ws 'col) (sb-alien:slot ws 'row))
          (values 80 24)))))

(defun terminal-columns ()
  (values (terminal-size)))

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

(defun goto (out row col)
  (format out "~C[~D;~DH" #\Esc row col))

(defun print-cell (out line width)
  "Print LINE, a (style . text) pair, padded to exactly WIDTH columns, style reset after."
  (format out "~C[~Am~A~C[0m" #\Esc (sgr (car line)) (pad (cdr line) width) #\Esc))

;;; Presentations: how objects become lines. The agent extends PRESENT by method.

(defgeneric present (object width)
  (:documentation "Lines, (style . text) pairs, showing OBJECT in WIDTH columns."))

(defun one-line (object)
  (let ((*print-length* 20) (*print-level* 3) (*print-pretty* nil))
    (first (text-lines (if (stringp object) object (prin1-to-string object))))))

(defun table-lines (rows width)
  "ROWS, lists of cells, as aligned columns; the first row is the header."
  (let* ((cells (mapcar (lambda (row) (mapcar #'one-line row)) rows))
         (count (reduce #'max cells :key #'length :initial-value 0))
         (widths (loop for column below count
                       collect (min (max 1 (floor width 2))
                                    (reduce #'max cells
                                            :key (lambda (row) (display-width (or (nth column row) "")))
                                            :initial-value 0)))))
    (loop for row in cells
          for index from 0
          collect (cons (if (zerop index) :user :plain)
                        (string-right-trim " "
                                           (format nil "~{~A~^  ~}"
                                                   (loop for cell in row for w in widths
                                                         collect (pad (or cell "") w))))))))

(defmethod present ((object string) width)
  (declare (ignore width))
  (mapcar (lambda (line) (cons :plain line)) (text-lines object)))

(defmethod present ((object null) width)
  (declare (ignore width))
  (list (cons :dim "nil")))

(defmethod present ((object hash-table) width)
  (declare (ignore width))
  (loop for key being the hash-keys of object using (hash-value value)
        collect (cons :plain (format nil "~A: ~A" (one-line key) (one-line value)))))

(defmethod present ((object list) width)
  (if (every #'listp object)
      (table-lines object width)
      (loop for element in object append (present element width))))

(defmethod present ((object t) width)
  (declare (ignore width))
  (mapcar (lambda (line) (cons :plain line)) (text-lines (prin1-to-string object))))

(defun present-safely (object width)
  (handler-case (present object width)
    (error (condition) (list (cons :error (format nil "present failed: ~A" condition))))))

;;; What the agent can do with the human.

(defun show (name object)
  "Open or update the pane called NAME, presenting OBJECT. Returns OBJECT."
  (when *ui* (sb-concurrency:send-message *ui* (list :show name object)))
  object)

(defun hide (name)
  "Close the pane called NAME."
  (when *ui* (sb-concurrency:send-message *ui* (list :hide name)))
  name)

(defun accept (type &key prompt options)
  "Ask the human and wait. TYPE is :boolean, answered with y or n; :string, answered with a
line; or :choice, answered with a number among OPTIONS. Returns the answer, nil when refused."
  (unless *ui* (error "no interface to ask"))
  (let ((reply (sb-concurrency:make-mailbox)))
    (sb-concurrency:send-message *ui* (list :accept (list :type type :prompt prompt :options options) reply))
    (sb-concurrency:receive-message reply)))
