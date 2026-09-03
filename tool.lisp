(in-package #:yuuki)

;;; SBCL keeps source for definitions evaluated in the default :compile evaluator
;;; mode, so `source` needs no bookkeeping of its own.

(defun truncate-result (text)
  "Cut TEXT to *MAX-RESULT-BYTES*, including an explicit truncation marker."
  (if (<= (length text) *max-result-bytes*)
      text
      (let* ((marker (format nil "... [truncated: ~D chars, cap ~D]"
                             (length text) *max-result-bytes*))
             (available (- *max-result-bytes* 1 (length marker))))
        (if (minusp available)
            (subseq marker 0 (min (length marker) *max-result-bytes*))
            (format nil "~A~%~A"
                    (subseq text 0 available)
                    marker)))))

(defun run-lisp (code &key (timeout 60))
  "Read and evaluate every form in CODE inside yuuki-user. Returns printed output,
each form's values REPL-style, and any error, cut at *max-result-bytes*."
  (let ((out (make-string-output-stream)))
    (handler-case
        (sb-ext:with-timeout timeout
          (let ((*package* (find-package '#:yuuki-user))
                (*standard-output* out)
                (*error-output* out)
                (*trace-output* out)
                (*standard-input* (make-string-input-stream "")))
            (with-input-from-string (in code)
              (loop for form = (read in nil in)
                    until (eq form in)
                    do (format out "~&~{=> ~S~%~}"
                               (multiple-value-list (eval form)))))))
      ;; boffin: keep timeout output separate so callers can distinguish it from
      ;; evaluation errors even though both stop the current run.
      (sb-ext:timeout ()
        (format out "~&error: timed out after ~A s~%" timeout))
      (error (condition)
        (format out "~&error: ~A~%" condition)))
    (truncate-result (get-output-stream-string out))))

(defun user-symbols ()
  (let ((package (find-package '#:yuuki-user))
        (symbols '()))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (push symbol symbols)))
    (sort symbols #'string< :key #'symbol-name)))

(defun definitions ()
  "Every function, macro and variable the agent has defined, with arglist and docstring."
  (with-output-to-string (out)
    (let ((*print-case* :downcase))
      (dolist (symbol (user-symbols))
        (when (fboundp symbol)
          (format out "(~A~{ ~A~})~@[  ; ~A~]~%" symbol
                  (sb-introspect:function-lambda-list
                   (or (macro-function symbol) (fdefinition symbol)))
                  (documentation symbol 'function)))
        (when (boundp symbol)
          (format out "~A = ~S~@[  ; ~A~]~%" symbol (symbol-value symbol)
                  (documentation symbol 'variable)))))))

(defun defun-form (name expression)
  "Turn (lambda args \"doc\" (block name . body)) back into a defun form."
  (destructuring-bind (lambda args &rest body) expression
    (declare (ignore lambda))
    (let* ((doc (and (stringp (first body)) (rest body) (first body)))
           (body (if doc (rest body) body))
           (block (first body)))
      (if (and (consp block) (eq (first block) 'block)
               (eq (second block) name))
          `(defun ,name ,args ,@(when doc (list doc)) ,@(cddr block))
          `(defun ,name ,args ,@(when doc (list doc)) ,@body)))))

(defun source (name)
  "The recorded source of the agent's function NAME, or a note that there is none."
  (let ((expression (and (fboundp name)
                         (function-lambda-expression
                          (or (macro-function name) (fdefinition name))))))
    (if expression
        (let ((*print-case* :downcase)
              (*package* (find-package '#:yuuki-user)))
          (with-output-to-string (out)
            (pprint (if (macro-function name)
                        expression
                        (defun-form name expression))
                    out)))
        (format nil "no source recorded for ~(~A~)" name))))
