;; This is a startup script to help load the web service

;; Make sure ASDF is loaded
(require :asdf)

;; Add the current directory to ASDF's search path
(push (truename ".") asdf:*central-registry*)

;; Install required dependencies first
(format t "~%Installing required dependencies...~%")
(ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities :fiveam :drakma) :silent t)
(format t "Dependencies installed successfully.~%")

;; Load our system
(format t "Loading the web service system...~%")
(asdf:load-system :sbcl-web-service)

;; Start the server
(format t "~%Starting the web service...~%")
(sbcl-web-service:main)


;; We'll use the get-env-var function from the sbcl-web-service package

;; Read environment variables from config.env file
(defparameter *env-vars* (sbcl-web-service::read-env-file #p"config.env"))

;; Print a helpful message
(format t "~%Web service is running. Access it at http://localhost:~a/~%" (parse-integer (sbcl-web-service::get-env-var *env-vars* "PORT" "8080")))
(format t "Available endpoints:~%")
(format t "  / - Hello World~%")
(format t "  /api/example - JSON API example~%")
(format t "  /health - Health check endpoint~%")
(format t "~%To stop the server, call (sbcl-web-service:stop-server)~%")
