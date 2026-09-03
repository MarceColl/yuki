(in-package #:yuuki)

;;; fx's Codex session, shared with fx.

(defvar *auth-path* nil
  "Session file to use. When nil, the codex CLI's file is used if present, else fx's.")

(defun auth-path ()
  (or *auth-path*
      (find-if #'probe-file
               (list (merge-pathnames ".codex/auth.json" (user-homedir-pathname))
                     (merge-pathnames ".fx/chatgpt-auth.json" (user-homedir-pathname))))))
(defparameter *client-id* "app_EMoamEEZ73f0CkXaXp7hrann")
(defparameter *token-url* "https://auth.openai.com/oauth/token")
(defparameter *responses-url* "https://chatgpt.com/backend-api/codex/responses")

(defun now-ms ()
  "Unix epoch milliseconds."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun jwt-payload (token)
  "The decoded payload object of a JWT."
  (let* ((parts (uiop:split-string token :separator "."))
         (payload (if (>= (length parts) 2)
                      (second parts)
                      (error "malformed JWT: expected header.payload.signature")))
         (padding (mod (- 4 (mod (length payload) 4)) 4))
         (padded (concatenate 'string payload (make-string padding :initial-element #\=)))
         (standard (substitute #\/ #\_ (substitute #\+ #\- padded))))
    (com.inuoe.jzon:parse
     (sb-ext:octets-to-string (cl-base64:base64-string-to-usb8-array standard)
                              :external-format :utf-8))))

(defun jwt-account-id (token)
  "The ChatGPT account id carried by a JWT access token."
  (or (path (jwt-payload token) "https://api.openai.com/auth" "chatgpt_account_id")
      (error "access token carries no chatgpt_account_id")))

(defun merged-session (session reply now-ms)
  "SESSION updated from a token endpoint REPLY. Pure; errors if the account changed."
  (let* ((access (or (gethash "access_token" reply) (error "token reply has no access_token")))
         (account (jwt-account-id access))
         (expires-in (gethash "expires_in" reply))
         (merged (alexandria:copy-hash-table session)))
    (unless (equal account (gethash "account_id" session))
      (error "codex account changed from ~A to ~A" (gethash "account_id" session) account))
    (setf (gethash "access_token" merged) access
          (gethash "refresh_token" merged) (or (gethash "refresh_token" reply) (gethash "refresh_token" session))
          (gethash "expires_at_ms" merged) (if expires-in
                                               (+ now-ms (* 1000 expires-in))
                                               (* 1000 (gethash "exp" (jwt-payload access)))))
    merged))

;;; Request.

(defparameter *tool*
  (obj "type" "function"
       "name" "lisp"
       "description" "Evaluate Common Lisp in the persistent yuuki-user package. Forms are read and evaluated in order; each form's values and everything printed come back. Definitions persist across calls and sessions. (definitions) lists what you have built, (source 'name) shows it. Build the helpers you need, files, shell via uiop:run-program, HTTP via dexador, libraries via ql:quickload, and reuse them."
       "strict" nil
       "parameters" (obj "type" "object"
                         "properties" (obj "code" (obj "type" "string")
                                           "timeout" (obj "type" "integer"
                                                          "description" "seconds before the evaluation is stopped, default 60"))
                         "required" (vector "code"))))

(defun request-body (instructions items)
  "The Codex Responses request as a JSON string."
  (com.inuoe.jzon:stringify
   (obj "model" *model* "store" nil "stream" t
        "instructions" instructions
        "input" (coerce items 'vector)
        "tools" (vector *tool*) "tool_choice" "auto" "parallel_tool_calls" t
        "include" (vector "reasoning.encrypted_content")
        "text" (obj "verbosity" "low")
        "reasoning" (obj "effort" *effort* "summary" "auto"))))

;;; Stream reduction.

(defun finish-reason (response)
  "Return the finish keyword represented by a completed Responses response."
  (let ((status (path response "status"))
        (reason (path response "incomplete_details" "reason")))
    (cond ((equal status "completed") :stop)
          ((and (equal status "incomplete") (equal reason "max_output_tokens")) :length)
          ((and (equal status "incomplete") (equal reason "content_filter")) :content-filter)
          ((equal status "incomplete") :stop)
          (t :failed))))

(defun classify (event)
  "One decoded SSE event to (values kind payload); kind is nil for events we ignore."
  (let ((type (gethash "type" event)))
    (flet ((one-of (&rest types) (member type types :test #'equal)))
      (cond ((one-of "response.output_text.delta" "response.refusal.delta")
             (values :text (gethash "delta" event)))
            ((one-of "response.reasoning_summary_text.delta" "response.reasoning_text.delta")
             (values :reasoning (gethash "delta" event)))
            ((one-of "response.reasoning_summary_part.done")
             (values :reasoning (format nil "~%~%")))
            ((one-of "response.output_item.done")
             (values :item (gethash "item" event)))
            ((one-of "response.completed" "response.done" "response.incomplete")
             (values :finish (finish-reason (gethash "response" event))))
            ((one-of "response.failed" "error")
             (values :finish :failed))
            (t (values nil nil))))))

;;; Session file.

(defun read-json-file (pathname)
  (com.inuoe.jzon:parse (uiop:read-file-string pathname)))

(defun write-json-file (pathname value)
  "Write VALUE as JSON to PATHNAME through a temp file and rename."
  (let ((temp (format nil "~A.tmp" (namestring pathname))))
    (with-open-file (out temp :direction :output :if-exists :supersede :external-format :utf-8)
      (com.inuoe.jzon:stringify value :stream out))
    (sb-posix:rename temp (namestring pathname))))

(defun refresh-session (session)
  (let ((reply (com.inuoe.jzon:parse
                (dex:post *token-url*
                          :headers '(("content-type" . "application/json"))
                          :content (com.inuoe.jzon:stringify
                                    (obj "client_id" *client-id*
                                         "grant_type" "refresh_token"
                                         "refresh_token" (gethash "refresh_token" session)))))))
    (merged-session session reply (now-ms))))

(defun session-of-file (file)
  "The session in fx's shape, from either fx's file or the codex CLI's (tokens under \"tokens\")."
  (let ((tokens (gethash "tokens" file)))
    (if (hash-table-p tokens)
        (obj "access_token" (gethash "access_token" tokens)
             "refresh_token" (gethash "refresh_token" tokens)
             "account_id" (gethash "account_id" tokens)
             "expires_at_ms" (* 1000 (gethash "exp" (jwt-payload (gethash "access_token" tokens)))))
        file)))

(defun iso-now ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))

(defun file-of-session (file session)
  "FILE as read, updated with SESSION's tokens, in the file's own shape. Pure."
  (let ((tokens (gethash "tokens" file)))
    (if (hash-table-p tokens)
        (let ((new-file (alexandria:copy-hash-table file))
              (new-tokens (alexandria:copy-hash-table tokens)))
          (setf (gethash "access_token" new-tokens) (gethash "access_token" session)
                (gethash "refresh_token" new-tokens) (gethash "refresh_token" session)
                (gethash "tokens" new-file) new-tokens
                (gethash "last_refresh" new-file) (iso-now))
          new-file)
        session)))

(defun session ()
  "The Codex session, refreshed and written back to its file when within a minute of expiry."
  (let ((path (auth-path)))
    (unless (and path (probe-file path))
      (error "no Codex session; run `codex login` or `fx login codex` first"))
    (let* ((file (read-json-file path))
           (session (session-of-file file)))
      (if (< (gethash "expires_at_ms" session) (+ (now-ms) 60000))
          (let ((fresh (refresh-session session)))
            (write-json-file path (file-of-session file fresh))
            fresh)
          session))))

;;; Streaming.

(defun http-failure-text (condition)
  (let ((body (dex:response-body condition)))
    (format nil "codex ~A: ~A" (dex:response-status condition)
            (if (streamp body) (alexandria:read-stream-content-into-string body) body))))

(defun reduce-sse (stream emit)
  "Consume SSE lines from STREAM until a terminal event, cancellation, or end of stream.
EMIT receives (:text delta) and (:reasoning delta). Returns output items in order and a finish keyword."
  (let ((output '()) (finish nil))
    (loop for line = (read-line stream nil)
          while (and line (null finish) (not *cancel*))
          do (when (uiop:string-prefix-p "data:" line)
               (let ((data (string-left-trim " " (subseq line (min 5 (length line))))))
                 (when (uiop:string-prefix-p "{" data)
                   (let ((event (handler-case (com.inuoe.jzon:parse data)
                                  (error () nil))))
                     (when event
                       (multiple-value-bind (kind payload) (classify event)
                         (case kind
                           ((:text :reasoning) (funcall emit (list kind payload)))
                           (:item (push payload output))
                           (:finish (setf finish payload))))))))))
    (values (nreverse output) (or finish (if *cancel* :cancelled :failed)))))

(defun stream-turn (instructions items emit)
  "POST one Responses request and reduce its stream. See reduce-sse for the return values."
  (let* ((session (session))
         (stream (handler-case
                     (dex:post *responses-url*
                               :headers `(("authorization" . ,(format nil "Bearer ~A" (gethash "access_token" session)))
                                          ("chatgpt-account-id" . ,(gethash "account_id" session))
                                          ("originator" . "yuuki")
                                          ("openai-beta" . "responses=experimental")
                                          ("accept" . "text/event-stream")
                                          ("content-type" . "application/json"))
                               :content (request-body instructions items)
                               :want-stream t :keep-alive nil)
                   (dex:http-request-failed (condition) (error "~A" (http-failure-text condition))))))
    (unwind-protect (reduce-sse stream emit)
      (close stream))))
