(defsystem "sbcl-cli-project"
  :version "0.0.1"
  :author "Mingyu Choo"
  :license "BSD-3-Clause license"
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "main"))))
  :description "A Project for Lisp"
  :in-order-to ((test-op (test-op "sbcl-cli-project-tests")))
  :build-operation "program-op"
  :build-pathname "sbcl-cli-project"
  :entry-point "sbcl-cli-project:main")
