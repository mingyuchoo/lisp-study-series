;;;; SBCL Web Service Initialize Script

;; This script loads and starts the web service application
;; It handles dependency loading, system initialization, and provides helpful information

;;;; System Setup

;; Make sure ASDF is loaded
(require :asdf)

;; Add the current directory to ASDF's search path
(push (truename ".") asdf:*central-registry*)

;; Define a flag to track overall success
(defvar *initialize-success* t)

;;;; Dependency Management

(format t "~%Installing required dependencies...~%")
(handler-case
    (progn
      (ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities :split-sequence :fiveam :drakma) :silent t)
      (format t "Dependencies installed successfully.~%"))
  (error (e)
    (format *error-output* "Error installing dependencies: ~A~%" e)
    (format t "Failed to install dependencies. Please check your Quicklisp installation.~%")
    (setf *initialize-success* nil)))

;;;; System Loading

;; Only proceed if dependencies loaded successfully
(when *initialize-success*
  (format t "Loading the web service system...~%")
  (handler-case
      (asdf:load-system :sbcl-web-service)
    (error (e)
      (format *error-output* "Error loading system: ~A~%" e)
      (format t "Failed to load the system. Please check for errors in the codebase.~%")
      (setf *initialize-success* nil))))

;;;; Application Initialize

;; Only proceed if system loaded successfully
(when *initialize-success*
  (format t "~%Starting the web service...~%")
  (let ((server (sbcl-web-service:main)))
    (unless server
      (format t "Failed to start the server. Check the logs for errors.~%")
      (setf *initialize-success* nil))))

;;;; Server Information

;; Only display server information if everything started successfully
(when *initialize-success*
  (let* ((env-vars (sbcl-web-service:read-env-file #p"config.env"))
         (port (sbcl-web-service:get-env-var env-vars "PORT" "8080"))
         (host (sbcl-web-service:get-env-var env-vars "IP" "127.0.0.1")))
    
    ;; Print a helpful message with server information
    (format t "~%Web service is running. Access it at http://~A:~A/~%" 
            (if (string= host "0.0.0.0") "localhost" host)
            port)
    (format t "Available endpoints:~%")
    (format t "  / - Hello World~%")
    (format t "  /api/example - JSON API example~%")
    (format t "  /api/health - Health check endpoint~%")
    (format t "~%To stop the server, call (sbcl-web-service:stop-server)~%")))
