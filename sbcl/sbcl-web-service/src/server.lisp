(in-package :sbcl-web-service)

;; Define a global variable to store our server instance
(defvar *acceptor* nil)

;; Function to start the server
(defun start-server (&optional (port nil))
  (let ((server-port (or port (getf (getf *config* :server) :port))))
    (format t "Starting the SBCL web service on port ~A...~%" server-port)
    
    ;; Stop the server if it's already running
    (when *acceptor*
      (format t "Server is already running. Stopping it first...~%")
      (stop-server))
    
    ;; Create and start a new server instance with configuration
    (setf *acceptor* 
          (make-instance 'hunchentoot:easy-acceptor 
                         :port server-port
                         :address (getf (getf *config* :server) :address)
                         ;; Don't set document-root to prevent automatic static file serving
                         ;; We'll handle all routing explicitly in routes.lisp
                         :access-log-destination (getf (getf *config* :server) :access-log-destination)
                         :message-log-destination (getf (getf *config* :server) :message-log-destination)))
    
    (hunchentoot:start *acceptor*)
    
    (format t "Server started successfully on port ~A!~%" server-port)
    (format t "Visit http://localhost:~A/ to see the application~%" server-port)
    *acceptor*))

;; Function to stop the server
(defun stop-server ()
  (when *acceptor*
    (format t "Stopping the server...~%")
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped.~%")))

;; Main function to start the application
(defun main ()
  (start-server))
