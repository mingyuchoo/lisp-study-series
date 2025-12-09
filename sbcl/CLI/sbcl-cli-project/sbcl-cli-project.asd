(defsystem "sbcl-cli-project"
  :version "0.0.1"
  :author "Mingyu Choo"
  :license "BSD-3-Clause license"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Project for Lisp"
  :build-operation "program-op"
  :build-pathname "sbcl-cli-project"
  :entry-point "sbcl-cli-project:main")
