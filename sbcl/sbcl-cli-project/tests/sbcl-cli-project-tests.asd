(defsystem "sbcl-cli-project-tests"
  :version "0.0.1"
  :author "Mingyu Choo"
  :license "BSD-3-Clause license"
  :depends-on ("sbcl-cli-project"
               "fiveam")
  :components ((:file "main-tests"))
  :description "Test system for sbcl-cli-project"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :sbcl-cli-project-tests
                                               :sbcl-cli-project-tests))))
