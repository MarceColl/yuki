;;;; The CAPI interface, LispWorks only: a transcript, a prompt line and a listener,
;;;; with show and accept as native panels and dialogs.

(defpackage #:yuuki-capi
  (:use #:cl #:yuuki)
  (:export #:start))

(in-package #:yuuki-capi)

(capi:define-interface window ()
  ((history :initform '() :accessor turn-history)
   (running :initform nil :accessor running))
  (:panes
   (transcript capi:collector-pane
               :reader transcript
               :visible-min-width 600
               :visible-min-height 500)
   (prompt capi:text-input-pane
           :reader prompt
           :callback 'submit
           :callback-type :interface
           :visible-min-width 600)
   (listener capi:listener-pane
             :reader listener
             :visible-min-width 500
             :visible-min-height 500))
  (:layouts
   (chat capi:column-layout '(transcript prompt) :y-ratios '(1 nil))
   (columns capi:row-layout '(chat listener) :x-ratios '(3 2)))
  (:default-initargs
   :title "yuuki"
   :layout 'columns
   :best-width 1400
   :best-height 900))

(defvar *window* nil "The open window, for show and accept.")

(defun say (window text)
  "Append TEXT to the transcript from any process."
  (capi:apply-in-pane-process
   (transcript window)
   (lambda ()
     (let ((stream (capi:collector-pane-stream (transcript window))))
       (write-string text stream)
       (force-output stream)))))

(defun handle (window event)
  (destructuring-bind (kind &rest args) event
    (case kind
      (:text (say window (first args)))
      (:reasoning nil)
      (:error (say window (first args)))
      (:review (say window (format nil "review: ~(~A~)~@[, ~A~]~%" (first args)
                                   (and (plusp (length (second args))) (second args)))))
      (:call (say window (format nil "~%~A~%" (second args))))
      (:result (say window (format nil "~A~%" (second args)))))))

(defun submit (window)
  "Enter in the prompt: run one turn on its own process."
  (let ((text (string-trim " " (capi:text-input-pane-text (prompt window)))))
    (when (and (plusp (length text)) (not (running window)))
      (setf (running window) t
            (capi:text-input-pane-text (prompt window)) "")
      (say window (format nil "> ~A~%" text))
      (mp:process-run-function
       "yuuki agent" '()
       (lambda ()
         (unwind-protect
              (handler-case
                  (setf (turn-history window)
                        (yuuki::run-turn (turn-history window) text
                                         :emit (lambda (event) (handle window event))
                                         :approve (lambda (id code)
                                                    (declare (ignore id))
                                                    (case yuuki:*permission*
                                                      (:yolo t)
                                                      (t (capi:confirm-yes-or-no "Run this?~%~%~A" code))))))
                (error (condition) (say window (format nil "error: ~A~%" condition))))
           (say window (format nil "~%"))
           (setf (running window) nil)))))))

;;; What the agent can do with the human, as native windows and dialogs.

(defun table-p (object)
  (and (consp object) (every #'listp object)))

(defun cell (value)
  (if (stringp value) value (prin1-to-string value)))

(defun yuuki:show (name object &key on-select)
  "Open a window called NAME presenting OBJECT: a table as columns, a list of atoms as a
list, anything else as text. With ON-SELECT, double-clicking a row calls it with the row."
  (capi:contain
   (cond ((table-p object)
          (make-instance 'capi:multi-column-list-panel
                         :columns (mapcar (lambda (title) (list :title (cell title))) (first object))
                         :items (rest object)
                         :column-function (lambda (row) (mapcar #'cell row))
                         :action-callback (and on-select (lambda (row pane) (declare (ignore pane)) (funcall on-select row)))
                         :callback-type :item-interface
                         :visible-min-width 500 :visible-min-height 300))
         ((and (consp object) (notany #'listp object))
          (make-instance 'capi:list-panel
                         :items object
                         :print-function #'cell
                         :action-callback (and on-select (lambda (item pane) (declare (ignore pane)) (funcall on-select item)))
                         :callback-type :item-interface
                         :visible-min-width 400 :visible-min-height 300))
         (t (make-instance 'capi:editor-pane
                           :text (if (stringp object) object (with-output-to-string (out) (pprint object out)))
                           :enabled :read-only
                           :visible-min-width 500 :visible-min-height 300)))
   :title (format nil "~A" name))
  object)

(defun yuuki:hide (name)
  (declare (ignore name))
  nil)

(defun yuuki:accept (type &key prompt options)
  "Ask the human with a dialog: :boolean, :string, or :choice among OPTIONS."
  (ecase type
    (:boolean (capi:confirm-yes-or-no "~A" (or prompt "?")))
    (:string (multiple-value-bind (text ok) (capi:prompt-for-string (or prompt "?"))
               (and ok text)))
    (:choice (multiple-value-bind (item ok) (capi:prompt-with-list options (or prompt "?") :print-function #'cell)
               (and ok item)))))

(defun start ()
  "Open the yuuki window: configure, open the store, load the agent's definitions."
  (yuuki::configure)
  (yuuki::open-store)
  (yuuki::load-definitions)
  (setf *window* (capi:display (make-instance 'window))))
