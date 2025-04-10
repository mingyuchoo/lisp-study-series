;;;; package.lisp
;;;; Defines the package structure for our Common Lisp web application

(defpackage :cl-webapp
  (:use :cl :hunchentoot :cl-who :parenscript :cl-css)
  (:export :start-server
           :stop-server
           :restart-server
           :*server*
           :*application-root*
           :print-startup-banner))
