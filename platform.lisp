;;;; The few implementation-specific pieces the core needs, for SBCL and LispWorks.

(in-package #:yuuki)

;;; Locks.

(defun make-lock (name)
  #+sbcl (sb-thread:make-mutex :name name)
  #+lispworks (mp:make-lock :name name :recursivep t))

(defmacro with-lock ((lock) &body body)
  #+sbcl `(sb-thread:with-recursive-lock (,lock) ,@body)
  #+lispworks `(mp:with-lock (,lock) ,@body))

;;; Time, bytes, files.

(defun now-ms ()
  "Unix epoch milliseconds, to the second."
  (* 1000 (- (get-universal-time) 2208988800)))

(defun octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun string-to-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun md5-hex (string)
  "Upper-case hex MD5 of STRING's UTF-8 bytes."
  (string-upcase (ironclad:byte-array-to-hex-string
                  (ironclad:digest-sequence :md5 (string-to-octets string)))))

(defun replace-file (from to)
  "Move FROM over TO, atomically where the implementation allows."
  #+sbcl (sb-posix:rename (namestring from) (namestring to))
  #-sbcl (progn (when (probe-file to) (delete-file to))
                (rename-file from to)))

;;; Timeouts.

(define-condition timed-out (error)
  ((seconds :initarg :seconds :reader timed-out-seconds))
  (:report (lambda (condition stream)
             (format stream "timed out after ~A s" (timed-out-seconds condition)))))

(defmacro with-timeout ((seconds) &body body)
  "Run BODY, signalling timed-out when it takes longer than SECONDS."
  #+sbcl `(handler-case (sb-ext:with-timeout ,seconds ,@body)
            (sb-ext:timeout () (error 'timed-out :seconds ,seconds)))
  #+lispworks `(run-with-timeout ,seconds (lambda () ,@body)))

#+lispworks
(defun run-with-timeout (seconds thunk)
  "Run THUNK in this process; a timer interrupts it with timed-out after SECONDS.
Without multiprocessing, as in a build script, run it with no limit."
  (unless (mp:multiprocessing-p) (return-from run-with-timeout (funcall thunk)))
  (let ((timer (mp:make-timer #'mp:process-interrupt (mp:get-current-process)
                              (lambda () (error 'timed-out :seconds seconds)))))
    (mp:schedule-timer-relative timer seconds)
    (unwind-protect (funcall thunk)
      (mp:unschedule-timer timer))))

;;; Introspection.

(defun function-arglist (function)
  #+sbcl (sb-introspect:function-lambda-list function)
  #+lispworks (lw:function-lambda-list function))

(defmacro muffling-redefinitions (&body body)
  #+sbcl `(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)) ,@body)
  #+lispworks `(let ((dspec:*redefinition-action* :quiet)) ,@body))
