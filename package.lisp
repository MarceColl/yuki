(defpackage #:yuuki
  (:use #:cl)
  (:lock t)
  (:export #:main #:save-image #:run-lisp #:definitions #:source
           #:*model* #:*effort* #:*permission* #:*max-steps* #:*max-result-bytes*))

(defpackage #:yuuki-user
  (:use #:cl #:uiop #:yuuki)
  (:documentation "The agent's package. Everything it defines lives here and is saved with the image."))

(in-package #:yuuki)

(defvar *model* nil "Codex model id, for example gpt-5.6-luna.")
(defvar *effort* "high" "Reasoning effort sent to the model.")
(defvar *permission* :ask "One of :ask or :yolo.")
(defvar *max-steps* 100 "Model calls allowed per turn.")
(defvar *max-result-bytes* 65536 "Tool results longer than this are cut.")
(defvar *cancel* nil "Set to true to stop the running turn at its next boundary.")

(defun obj (&rest kv)
  "A JSON object (equal hash table) from alternating string keys and values."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on kv by #'cddr do (setf (gethash key table) value))
    table))

(defun path (table &rest keys)
  "Walk nested JSON objects by KEYS; nil when any step is missing."
  (loop for key in keys
        while (hash-table-p table)
        do (setf table (gethash key table))
        finally (return table)))
