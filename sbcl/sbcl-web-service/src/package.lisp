(defpackage :sbcl-web-service
  (:documentation "A web service built with Steel Bank Common Lisp.")
  (:use :cl :split-sequence
        :sbcl-web-service.core
        :sbcl-web-service.utils
        :sbcl-web-service.api
        :sbcl-web-service.web)
  (:export ;; Main entry point
           :main
           ;; Re-export core functionality
           :start-server 
           :stop-server
           :*config*
           :get-env-var
           :read-env-file))

(in-package :sbcl-web-service)
