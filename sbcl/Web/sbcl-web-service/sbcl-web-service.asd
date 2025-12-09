(defsystem "sbcl-web-service"
  :description "A web service built with Steel Bank Common Lisp."
  :author "mingyuchoo"
  :license "BSD 3-Clause"
  :depends-on ("hunchentoot" "cl-json" "alexandria" "cl-ppcre" "cl-utilities" "split-sequence")
  :components ((:module "src"
                :serial t
                :components
                ((:module "utils"
                  :serial t
                  :components ((:file "package")
                             (:file "utils")))
                 (:module "core"
                  :serial t
                  :components ((:file "package")
                             (:file "config")
                             (:file "server")))
                 (:module "web"
                  :serial t
                  :components ((:file "package")
                             (:file "routes")))
                 (:module "api"
                  :serial t
                  :components ((:file "package")
                             (:file "routes")))
                 (:file "package")
                 (:file "main")))
               (:module "tests"
               :components ((:file "test-suite")))))

(defsystem "sbcl-web-service/tests"
  :description "Test system for sbcl-web-service"
  :author "mingyuchoo"
  :license "BSD 3-Clause"
  :depends-on ("sbcl-web-service" "fiveam" "drakma")
  :components ((:module "tests"
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (symbol-call :sbcl-web-service.tests :run-tests)))