(in-package #:yuuki/test)

(in-suite :yuuki)

(defun fake-jwt (payload-json)
  "header.payload.signature with a base64url payload and no padding."
  (let ((payload (string-right-trim "="
                   (cl-base64:usb8-array-to-base64-string
                    (yuuki::string-to-octets payload-json) :uri t))))
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
    (multiple-value-bind (value present) (gethash "store" body)
      (is (eq t present)) (is (null value)))
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

(defparameter *sse-sample*
  (format nil "~{~A~%~}"
          '("event: response.created"
            "data: {\"type\":\"response.created\",\"response\":{\"id\":\"r1\"}}"
            ""
            "data: {\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs1\",\"encrypted_content\":\"ZZZ\",\"summary\":[]}}"
            ""
            "data: {\"type\":\"response.output_text.delta\",\"delta\":\"working\"}"
            ""
            "data: {\"type\":\"response.output_item.done\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"id\":\"fc1\",\"call_id\":\"c1\",\"name\":\"lisp\",\"arguments\":\"{\\\"code\\\":\\\"1\\\"}\"}}"
            ""
            "data: {\"type\":\"response.completed\",\"response\":{\"status\":\"completed\",\"usage\":{\"input_tokens\":1}}}"
            ""
            "data: {\"type\":\"response.never_seen\"}")))

(test reduce-sse-collects-items-in-order-and-stops-at-terminal
  (let ((events '()))
    (multiple-value-bind (items finish)
        (with-input-from-string (in *sse-sample*)
          (yuuki::reduce-sse in (lambda (e) (push e events))))
      (is (eq :stop finish))
      (is (= 2 (length items)))
      (is (string= "reasoning" (gethash "type" (first items))))
      (is (string= "ZZZ" (gethash "encrypted_content" (first items))))
      (is (string= "c1" (gethash "call_id" (second items))))
      (is (equal '((:text "working")) events)))))

(test character-stream-wraps-octet-streams
  (uiop:with-temporary-file (:pathname temp :type "bin")
    (with-open-file (out temp :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence (yuuki::string-to-octets (format nil "data: {\"type\":\"response.output_text.delta\",\"delta\":\"héllo\"}~%")) out))
    (with-open-file (in temp :element-type '(unsigned-byte 8))
      (let ((events '()))
        (multiple-value-bind (items finish)
            (yuuki::reduce-sse (yuuki::character-stream in) (lambda (e) (push e events)))
          (is (null items))
          (is (eq :failed finish))
          (is (equal (list (list :text "héllo")) events))))))
  (with-input-from-string (in "x")
    (is (eq in (yuuki::character-stream in)))))

(test reduce-sse-without-terminal-is-failed
  (multiple-value-bind (items finish)
      (with-input-from-string (in (format nil "data: {\"type\":\"response.output_text.delta\",\"delta\":\"x\"}~%"))
        (yuuki::reduce-sse in (lambda (e) (declare (ignore e)))))
    (is (null items))
    (is (eq :failed finish))))

(test reduce-sse-honours-cancel
  (let ((yuuki::*cancel* t))
    (multiple-value-bind (items finish)
        (with-input-from-string (in *sse-sample*)
          (yuuki::reduce-sse in (lambda (e) (declare (ignore e)))))
      (is (null items))
      (is (eq :cancelled finish)))))

(test session-returns-file-when-fresh
  (uiop:with-temporary-file (:pathname temp :type "json")
    (let ((far (+ (yuuki::now-ms) (* 24 3600 1000))))
      (with-open-file (out temp :direction :output :if-exists :supersede)
        (format out "{\"access_token\":\"a\",\"refresh_token\":\"r\",\"expires_at_ms\":~D,\"account_id\":\"acct\",\"version\":1}" far))
      (let* ((yuuki::*auth-path* temp)
             (session (yuuki::session)))
        (is (string= "a" (gethash "access_token" session)))
        (is (= far (gethash "expires_at_ms" session)))))))

(test session-of-file-reads-codex-cli-shape
  (let* ((token (fake-jwt "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct_1\"},\"exp\":1800}"))
         (file (yuuki::obj "auth_mode" "chatgpt" "last_refresh" "old"
                           "tokens" (yuuki::obj "id_token" "i" "access_token" token
                                                "refresh_token" "r0" "account_id" "acct_1")))
         (session (yuuki::session-of-file file)))
    (is (string= token (gethash "access_token" session)))
    (is (string= "r0" (gethash "refresh_token" session)))
    (is (string= "acct_1" (gethash "account_id" session)))
    (is (= 1800000 (gethash "expires_at_ms" session)))
    (let ((written (yuuki::file-of-session file (yuuki::obj "access_token" "new" "refresh_token" "r1"))))
      (is (string= "chatgpt" (gethash "auth_mode" written)))
      (is (string= "i" (yuuki::path written "tokens" "id_token")))
      (is (string= "new" (yuuki::path written "tokens" "access_token")))
      (is (string= "r1" (yuuki::path written "tokens" "refresh_token")))
      (is (not (string= "old" (gethash "last_refresh" written))))
      (is (string= "old" (gethash "last_refresh" file)))
      (is (string= token (yuuki::path file "tokens" "access_token"))))))

(test session-of-file-passes-fx-shape-through
  (let ((file (yuuki::obj "access_token" "a" "refresh_token" "r" "expires_at_ms" 5 "account_id" "x" "version" 1)))
    (is (eq file (yuuki::session-of-file file)))
    (let ((fresh (yuuki::obj "access_token" "b")))
      (is (eq fresh (yuuki::file-of-session file fresh))))))

(test session-reads-codex-cli-file-when-fresh
  (uiop:with-temporary-file (:pathname temp :type "json")
    (let* ((far (+ (floor (yuuki::now-ms) 1000) (* 24 3600)))
           (token (fake-jwt (format nil "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct\"},\"exp\":~D}" far))))
      (with-open-file (out temp :direction :output :if-exists :supersede)
        (format out "{\"auth_mode\":\"chatgpt\",\"tokens\":{\"access_token\":~S,\"refresh_token\":\"r\",\"account_id\":\"acct\"}}" token))
      (let* ((yuuki::*auth-path* temp)
             (session (yuuki::session)))
        (is (string= token (gethash "access_token" session)))
        (is (= (* 1000 far) (gethash "expires_at_ms" session)))))))

(test session-signals-clearly-when-file-missing
  (let ((yuuki::*auth-path* #p"/nonexistent/chatgpt-auth.json"))
    (signals error (yuuki::session))))
