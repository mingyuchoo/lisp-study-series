(defsystem "sbcl-web-service"
  :description "A web service built with Steel Bank Common Lisp."
  :author "mingyuchoo"
  :license "BSD 3-Clause"
  :depends-on ("hunchentoot" "cl-json" "alexandria" "cl-ppcre" "cl-utilities")
  :components ((:module "src"
               :serial t
               :components ((:file "package")
                           (:file "config")
                           (:file "utils")
                           (:file "server")
                           (:file "routes")
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