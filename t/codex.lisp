(in-package #:yuuki/test)

(in-suite :yuuki)

(defun fake-jwt (payload-json)
  "header.payload.signature with a base64url payload and no padding."
  (let ((payload (string-right-trim "="
                   (cl-base64:usb8-array-to-base64-string
                    (sb-ext:string-to-octets payload-json :external-format :utf-8) :uri t))))
    (format nil "eyJhbGciOiJub25lIn0.~A.sig" payload)))

(test jwt-account-id-reads-auth-claim
  (let ((token (fake-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_1\"},\"exp\":1700000000}")))
    (is (string= "acct_1" (yuuki::jwt-account-id token)))
    (is (= 1700000000 (gethash "exp" (yuuki::jwt-payload token))))))

(test merged-session-keeps-shape-and-rotates
  (let* ((session (yuuki::obj "access_token" "old" "refresh_token" "r0"
                              "expires_at_ms" 1 "account_id" "acct_1" "version" 1))
         (token (fake-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_1\"},\"exp\":2000}"))
         (reply (yuuki::obj "access_token" token "refresh_token" "r1" "expires_in" 3600))
         (merged (yuuki::merged-session session reply 1000)))
    (is (string= token (gethash "access_token" merged)))
    (is (string= "r1" (gethash "refresh_token" merged)))
    (is (= (+ 1000 3600000) (gethash "expires_at_ms" merged)))
    (is (= 1 (gethash "version" merged)))
    (is (string= "old" (gethash "access_token" session)))))

(test merged-session-falls-back-to-exp-and-old-refresh
  (let* ((session (yuuki::obj "access_token" "old" "refresh_token" "r0"
                              "expires_at_ms" 1 "account_id" "acct_1" "version" 1))
         (token (fake-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_1\"},\"exp\":2000}"))
         (merged (yuuki::merged-session session (yuuki::obj "access_token" token) 1000)))
    (is (string= "r0" (gethash "refresh_token" merged)))
    (is (= 2000000 (gethash "expires_at_ms" merged)))))

(test merged-session-rejects-account-change
  (let* ((session (yuuki::obj "access_token" "old" "refresh_token" "r0"
                              "expires_at_ms" 1 "account_id" "acct_1" "version" 1))
         (token (fake-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_2\"},\"exp\":2000}")))
    (signals error (yuuki::merged-session session (yuuki::obj "access_token" token) 1000))))

(test request-body-has-codex-shape
  (let* ((yuuki:*model* "gpt-test") (yuuki:*effort* "high")
         (items (list (yuuki::obj "role" "user" "content" (vector (yuuki::obj "type" "input_text" "text" "hi")))))
         (body (com.inuoe.jzon:parse (yuuki::request-body "SYS" items))))
    (is (string= "gpt-test" (gethash "model" body)))
    (is (eq nil (gethash "store" body)))
    (is (eq t (gethash "stream" body)))
    (is (string= "SYS" (gethash "instructions" body)))
    (is (= 1 (length (gethash "input" body))))
    (is (string= "lisp" (gethash "name" (aref (gethash "tools" body) 0))))
    (is (string= "auto" (gethash "tool_choice" body)))
    (is (string= "reasoning.encrypted_content" (aref (gethash "include" body) 0)))
    (is (string= "high" (yuuki::path body "reasoning" "effort")))
    (is (null (gethash "max_output_tokens" body)))))

(defun event (json) (com.inuoe.jzon:parse json))

(test classify-maps-sse-events
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.output_text.delta\",\"delta\":\"hi\"}"))
    (is (eq :text kind)) (is (string= "hi" payload)))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"th\"}"))
    (is (eq :reasoning kind)) (is (string= "th" payload)))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.reasoning_summary_part.done\"}"))
    (is (eq :reasoning kind)) (is (string= (format nil "~%~%") payload)))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"lisp\",\"arguments\":\"{}\"}}"))
    (is (eq :item kind)) (is (string= "c1" (gethash "call_id" payload))))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.completed\",\"response\":{\"status\":\"completed\"}}"))
    (is (eq :finish kind)) (is (eq :stop payload)))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.incomplete\",\"response\":{\"status\":\"incomplete\",\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}"))
    (is (eq :finish kind)) (is (eq :length payload)))
  (multiple-value-bind (kind payload) (yuuki::classify (event "{\"type\":\"response.failed\"}"))
    (is (eq :finish kind)) (is (eq :failed payload)))
  (is (null (yuuki::classify (event "{\"type\":\"response.created\"}")))))
