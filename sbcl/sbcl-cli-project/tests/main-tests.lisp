(defpackage sbcl-cli-project-tests
  (:use :cl :fiveam)
  (:export :sbcl-cli-project-tests))

(in-package :sbcl-cli-project-tests)

;; Define test suite
(def-suite sbcl-cli-project-tests
  :description "Test suite for sbcl-cli-project")

(in-suite sbcl-cli-project-tests)

;; Test for main function
(test main-function-output
  "Test that main function outputs 'Hello, World!'"
  (let ((output (with-output-to-string (*standard-output*)
                  (sbcl-cli-project:main))))
    (is (string= "Hello, World!
" output))
    (is (not (string= "" output)))))

(test main-function-exists
  "Test that main function exists and is callable"
  (finishes (sbcl-cli-project:main)))

;; Run tests when loading this file directly
(defun run-tests ()
  "Run all tests in the sbcl-cli-project-tests suite"
  (run! 'sbcl-cli-project-tests))
