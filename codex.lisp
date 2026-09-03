(in-package #:yuuki)

;;; fx's Codex session, shared with fx.

(defparameter *auth-path* (merge-pathnames ".fx/chatgpt-auth.json" (user-homedir-pathname)))
(defparameter *client-id* "app_EMoamEEZ73f0CkXaXp7hrann")
(defparameter *token-url* "https://auth.openai.com/oauth/token")
(defparameter *responses-url* "https://chatgpt.com/backend-api/codex/responses")

(defun now-ms ()
  "Unix epoch milliseconds."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun jwt-payload (token)
  "The decoded payload object of a JWT."
  (let* ((payload (second (uiop:split-string token :separator ".")))
         (padding (mod (- 4 (mod (length payload) 4)) 4))
         (padded (concatenate 'string payload (make-string padding :initial-element #\=)))
         ;; boffin: Normalize URL-safe alphabet before the decoder sees padding.
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
         ;; boffin: Copy the session before replacing rotated credentials.
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
          ;; boffin: Keep the token-limit result ahead of generic incomplete stops.
          ((equal reason "max_output_tokens") :length)
          ((equal reason "content_filter") :content-filter)
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
