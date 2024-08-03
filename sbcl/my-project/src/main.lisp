(defpackage #:my-project
  (:use #:cl)
  (:export #:hello-world))

(in-package #:my-project)

(defun hello-world ()
  (format t "Hello, World!~%"))

