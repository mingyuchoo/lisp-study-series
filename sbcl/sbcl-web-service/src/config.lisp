(in-package :sbcl-web-service)

;; Configuration parameters
(defparameter *config* 
  '(:server (:port 8080
             :address "127.0.0.1"
             :document-root #p"./static/"
             :access-log-destination nil
             :message-log-destination nil)))
