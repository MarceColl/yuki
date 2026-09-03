(in-package #:yuuki)

;;; Every definition the agent evaluates is recorded in the store (store.lisp).
;;; For functions defined some other way, SBCL's retained lambda expression
;;; (default :compile evaluator mode) is the fallback for source.

(defun truncate-result (text)
  "Cut TEXT to *MAX-RESULT-BYTES*, including an explicit truncation marker."
  (if (<= (length text) *max-result-chars*)
      text
      (let* ((marker (format nil "... [truncated: ~D chars, cap ~D]"
                             (length text) *max-result-chars*))
             (available (- *max-result-chars* 1 (length marker))))
        (if (minusp available)
            (subseq marker 0 (min (length marker) *max-result-chars*))
            (format nil "~A~%~A"
                    (subseq text 0 available)
                    marker)))))

(defun run-lisp (code &key (timeout 60))
  "Read and evaluate every form in CODE inside yuuki-user. Returns printed output,
each form's values REPL-style, and any error, cut at *max-result-chars*."
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
                    do (let ((values (multiple-value-list (eval form))))
                         (record form)
                         (format out "~&~{=> ~S~%~}" values))))))
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

(defun state-block ()
  "The agent's definitions with current values, bounded, as the model sees them each step."
  (let ((*print-length* 50) (*print-level* 4))
    (let ((text (definitions)))
      (if (zerop (length text)) "nothing defined yet" (truncate-result text)))))

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

(defun pretty (form)
  (let ((*print-case* :downcase)
        (*package* (find-package '#:yuuki-user)))
    (with-output-to-string (out) (pprint form out))))

(defun source (name)
  "The source of the agent's definition NAME: the form it evaluated, else what SBCL retained."
  (let* ((form (stored-form name))
         (expression (and (null form) (fboundp name) (not (macro-function name))
                          (function-lambda-expression (fdefinition name)))))
    (cond (form (pretty form))
          (expression (pretty (defun-form name expression)))
          (t (format nil "no source recorded for ~(~A~)" name)))))
