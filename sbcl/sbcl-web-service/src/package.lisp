(defpackage :sbcl-web-service
  (:use :cl :split-sequence)
  (:export :start-server 
           :stop-server
           :main
           :*config*))

(in-package :sbcl-web-service)
