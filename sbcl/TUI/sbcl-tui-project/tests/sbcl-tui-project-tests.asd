(defsystem "sbcl-tui-project-tests"
  :version "0.0.1"
  :author ""
  :license "BSD-3-Clause license"
  :depends-on (:sbcl-tui-project)
  :components ((:file "main-tests"))
  :description "Test system for sbcl-tui-project"
  :perform (test-op (op c)
                    (symbol-call :sbcl-tui-project-tests :run-tests)))

