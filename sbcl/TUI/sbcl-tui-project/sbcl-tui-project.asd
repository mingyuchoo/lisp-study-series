(defsystem "sbcl-tui-project"
  :version "0.0.1"
  :author ""
  :license "BSD-3-Clause license"
  :depends-on ("croatoan")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "ui")
                 (:file "main"))))
  :description "A TUI Application built with croatoan"
  :build-operation "program-op"
  :build-pathname "sbcl-tui-project"
  :entry-point "sbcl-tui-project:main")

