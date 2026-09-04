(asdf:defsystem "yuuki"
  :description "A Common Lisp coding agent with one tool: the image itself. The portable core."
  :depends-on ("dexador" "com.inuoe.jzon" "cl-base64" "alexandria" "sqlite" "ironclad" "babel"
               #+sbcl (:require "sb-introspect"))
  :serial t
  :components ((:file "package")
               (:file "platform")
               (:file "store")
               (:file "codex")
               (:file "tool")
               (:file "hooks")
               (:file "context")
               (:file "agent"))
  :in-order-to ((test-op (test-op "yuuki/test"))))

(asdf:defsystem "yuuki/tui"
  :description "The terminal interface, SBCL only."
  :depends-on ("yuuki" (:require "sb-posix") (:require "sb-concurrency"))
  :serial t
  :components ((:file "ui")
               (:file "app")))

(asdf:defsystem "yuuki/capi"
  :description "The CAPI interface, LispWorks only."
  :depends-on ("yuuki")
  :components ((:file "capi")))

(asdf:defsystem "yuuki/core-test"
  :description "The tests that run on any implementation."
  :depends-on ("yuuki" "fiveam")
  :serial t
  :components ((:module "t"
                :components ((:file "suite")
                             (:file "package")
                             (:file "store")
                             (:file "codex")
                             (:file "tool")
                             (:file "hooks")
                             (:file "agent")))))

(asdf:defsystem "yuuki/test"
  :depends-on ("yuuki/tui" "yuuki/core-test")
  :serial t
  :components ((:module "t"
                :components ((:file "ui")
                             (:file "app"))))
  :perform (test-op (o c)
             (let ((results (uiop:symbol-call :fiveam :run :yuuki)))
               (uiop:symbol-call :fiveam :explain! results)
               (unless (uiop:symbol-call :fiveam :results-status results)
                 (error "tests failed")))))
