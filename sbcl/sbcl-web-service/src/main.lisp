;; Load Quicklisp first
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load required packages
(ql:quickload :hunchentoot)

(defpackage :sbcl-web-service
  (:use :cl :hunchentoot)
  (:export :start-server :stop-server))

(in-package :sbcl-web-service)

;; Define a global variable to store our server instance
(defvar *acceptor* nil)

;; Define a simple handler function that returns 'Hello, World!'
(define-easy-handler (hello-world :uri "/") ()
  (setf (content-type*) "text/plain")
  "Hello, World!")

;; Function to start the server
(defun start-server (&optional (port 8080))
  (format t "Starting the SBCL web service on port ~A...~%" port)
  
  ;; Stop the server if it's already running
  (when *acceptor*
    (format t "Server is already running. Stopping it first...~%")
    (stop-server))
  
  ;; Create and start a new server instance
  (setf *acceptor* (make-instance 'easy-acceptor :port port))
  (start *acceptor*)
  
  (format t "Server started successfully on port ~A!~%" port)
  (format t "Visit http://localhost:~A/ to see 'Hello, World!'~%" port))

;; Function to stop the server
(defun stop-server ()
  (when *acceptor*
    (format t "Stopping the server...~%")
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped.~%")))

;; Main function to start the application
(defun main ()
  (start-server 8080))

;; Start the server when this file is loaded
(main)