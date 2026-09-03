(asdf:defsystem "yuuki"
  :description "A Common Lisp coding agent with one tool: the image itself."
  :depends-on ("dexador" "com.inuoe.jzon" "cl-base64" "alexandria" "sqlite"
               (:require "sb-posix") (:require "sb-introspect") (:require "sb-concurrency") (:require "sb-md5"))
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "codex")
               (:file "tool")
               (:file "context")
               (:file "agent")
               (:file "ui")
               (:file "app"))
  :in-order-to ((test-op (test-op "yuuki/test"))))

(asdf:defsystem "yuuki/test"
  :depends-on ("yuuki" "fiveam")
  :serial t
  :components ((:module "t"
                :components ((:file "suite")
                             (:file "package")
                             (:file "store")
                             (:file "codex")
                             (:file "tool")
                             (:file "agent")
                             (:file "ui")
                             (:file "app"))))
  :perform (test-op (o c)
             (let ((results (uiop:symbol-call :fiveam :run :yuuki)))
               (uiop:symbol-call :fiveam :explain! results)
               (unless (uiop:symbol-call :fiveam :results-status results)
                 (error "tests failed")))))
