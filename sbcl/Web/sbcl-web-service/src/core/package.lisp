(defpackage :sbcl-web-service.core
  (:documentation "Core functionality for the SBCL web service.")
  (:use :cl :split-sequence)
  (:export ;; Configuration (pure)
           :*config*
           :*env-vars*
           :parse-env-line
           :get-env-var
           :read-env-file
           :parse-env-var-integer
           ;; Server - pure functions
           :extract-server-config
           :resolve-server-params
           :make-acceptor
           :display-address
           ;; Server - effect functions
           :start-acceptor
           :stop-acceptor
           ;; Server - stateful shell
           :start-server
           :stop-server
           :*acceptor*))

(in-package :sbcl-web-service.core)
