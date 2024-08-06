(defpackage my-project
  (:use :cl)
  (:export :main))
  
(in-package :my-project)

;; Main function
(defun main ()
  (format t "Hello, World!~%"))
