(defpackage sbcl-cli-project
  (:use :cl)
  (:export :main))
  
(in-package :sbcl-cli-project)

;; Main function
(defun main ()
  (format t "Hello, World!~%"))
