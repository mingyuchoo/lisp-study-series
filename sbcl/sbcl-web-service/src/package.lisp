(defpackage :sbcl-web-service
  (:documentation "A web service built with Steel Bank Common Lisp.")
  (:use :cl :split-sequence)
  (:export ;; Server management
           :start-server 
           :stop-server
           :main
           ;; Configuration
           :*config*
           :get-env-var
           :read-env-file))

(in-package :sbcl-web-service)
