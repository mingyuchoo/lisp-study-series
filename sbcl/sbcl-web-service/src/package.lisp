(defpackage :sbcl-web-service
  (:use :cl)
  (:export :start-server 
           :stop-server
           :main
           :*config*))

(in-package :sbcl-web-service)
