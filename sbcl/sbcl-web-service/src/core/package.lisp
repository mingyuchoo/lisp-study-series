(defpackage :sbcl-web-service.core
  (:documentation "Core functionality for the SBCL web service.")
  (:use :cl :split-sequence)
  (:export ;; Configuration
           :*config*
           :get-env-var
           :read-env-file
           ;; Server management
           :start-server 
           :stop-server
           :*acceptor*))

(in-package :sbcl-web-service.core)
