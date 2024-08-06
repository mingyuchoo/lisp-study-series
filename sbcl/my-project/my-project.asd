(defsystem "my-project"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "my-project"
  :entry-point "my-project:main")
