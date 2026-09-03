;;;; Definitions store, after rekishi (MarceColl/rekishi): every definition the
;;;; agent evaluates is kept in SQLite, content-addressed, with a link to the
;;;; version it replaced. Nothing else persists between runs.

(in-package #:yuuki)

(defvar *store-path* (merge-pathnames ".yuuki/definitions.sqlite3" (user-homedir-pathname))
  "SQLite file holding the agent's definitions and their history.")

(defvar *store* nil "Open store connection, or nil when nothing is recorded.")

(defun open-store (&optional (path *store-path*))
  "Open the definitions store at PATH, creating file and tables as needed."
  (ensure-directories-exist path)
  (let ((db (sqlite:connect (namestring path))))
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS objects (hash TEXT PRIMARY KEY, name TEXT, form TEXT, parent TEXT, mtime INTEGER)")
    (sqlite:execute-non-query db "CREATE TABLE IF NOT EXISTS bindings (name TEXT PRIMARY KEY, object TEXT REFERENCES objects (hash))")
    (setf *store* db)))

(defun close-store ()
  (when *store*
    (sqlite:disconnect *store*)
    (setf *store* nil)))

(defun definition-name (form)
  "NAME when FORM is a top-level definition such as (defun name ...) or (defstruct (name ...) ...)."
  (when (and (consp form) (symbolp (first form)) (macro-function (first form))
             (uiop:string-prefix-p "DEF" (symbol-name (first form))))
    (let ((name (second form)))
      (cond ((symbolp name) name)
            ((and (consp name) (symbolp (first name))) (first name))))))

(defun form-text (form)
  "FORM printed readably, relative to the agent's package."
  (let ((*package* (find-package '#:yuuki-user))
        (*print-case* :downcase) (*print-pretty* t) (*print-right-margin* 80))
    (prin1-to-string form)))

(defun read-form (text)
  (let ((*package* (find-package '#:yuuki-user)))
    (read-from-string text)))

(defun form-hash (text)
  (format nil "~{~2,'0x~}" (coerce (sb-md5:md5sum-string text :external-format :utf-8) 'list)))

(defun current-object (name)
  (sqlite:execute-single *store* "SELECT object FROM bindings WHERE name = ?" (symbol-name name)))

(defun bind (name hash)
  (sqlite:execute-non-query
   *store* "INSERT INTO bindings (name, object) VALUES (?, ?) ON CONFLICT (name) DO UPDATE SET object = excluded.object"
   (symbol-name name) hash))

(defun record (form)
  "Make FORM the current definition of its name, linked to the version it replaces.
A form identical to an existing version only rebinds. No-op without a store or
when FORM defines nothing. Returns the object hash."
  (let ((name (definition-name form)))
    (when (and *store* name)
      (let* ((text (form-text form))
             (hash (form-hash text))
             (current (current-object name)))
        (unless (equal hash current)
          (sqlite:execute-non-query
           *store* "INSERT INTO objects (hash, name, form, parent, mtime) VALUES (?, ?, ?, ?, strftime('%s', 'now')) ON CONFLICT (hash) DO NOTHING"
           hash (symbol-name name) text current)
          (bind name hash))
        hash))))

(defun object-form (hash)
  "The stored form under HASH, a full hash or a prefix, or nil."
  (let ((text (sqlite:execute-single *store* "SELECT form FROM objects WHERE hash LIKE ?" (concatenate 'string hash "%"))))
    (and text (read-form text))))

(defun stored-form (name)
  "The current stored definition of NAME, or nil."
  (let ((hash (and *store* (current-object name))))
    (and hash (object-form hash))))

(defun versions (name)
  "Every version of NAME's definition, newest first, as (hash mtime form-text current-p) rows."
  (when *store*
    (let ((current (current-object name)))
      (mapcar (lambda (row) (append row (list (equal (first row) current))))
              (sqlite:execute-to-list *store* "SELECT hash, mtime, form FROM objects WHERE name = ? ORDER BY mtime DESC, rowid DESC"
                                      (symbol-name name))))))

(defun history (name)
  "The versions of the agent's definition NAME, newest first, the current one starred."
  (let ((versions (versions name)))
    (if versions
        (with-output-to-string (out)
          (dolist (row versions)
            (format out "~:[ ~;*~] ~A  ~A~%~A~%~%" (fourth row) (subseq (first row) 0 10) (iso-time (second row)) (third row))))
        (format nil "no versions of ~(~A~)" name))))

(defun iso-time (unix-seconds)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (+ unix-seconds 2208988800) 0)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour minute second)))

(defun rollback (name &optional hash)
  "Re-evaluate an earlier definition of NAME: the one whose hash starts with HASH,
else the version before the current one. Rebinding creates no new version."
  (let* ((current (and *store* (current-object name)))
         (target (cond ((null current) nil)
                       (hash (sqlite:execute-single *store* "SELECT hash FROM objects WHERE name = ? AND hash LIKE ?"
                                                    (symbol-name name) (concatenate 'string hash "%")))
                       (t (sqlite:execute-single *store* "SELECT parent FROM objects WHERE hash = ?" current))))
         (form (and target (object-form target))))
    (unless form (error "no earlier version of ~(~A~)" name))
    (let ((*package* (find-package '#:yuuki-user))) (eval form))
    (bind name target)
    form))

(defun load-definitions ()
  "Evaluate every current definition from the store into the agent's package, macros
first. Returns how many loaded; a failing form is reported and skipped."
  (when *store*
    (let* ((forms (mapcar (lambda (row) (read-form (first row)))
                          (sqlite:execute-to-list *store* "SELECT o.form FROM bindings AS b JOIN objects AS o ON o.hash = b.object ORDER BY b.rowid")))
           (ordered (stable-sort (copy-list forms) #'> :key (lambda (form) (if (eq (first form) 'defmacro) 1 0))))
           (count 0))
      (dolist (form ordered count)
        (handler-case
            (handler-bind ((warning #'muffle-warning))
              (let ((*package* (find-package '#:yuuki-user))) (eval form))
              (incf count))
          (error (condition)
            (format *error-output* "yuuki: could not load ~(~A~): ~A~%" (definition-name form) condition)))))))
