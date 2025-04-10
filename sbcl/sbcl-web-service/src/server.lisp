(in-package :sbcl-web-service)

;;;; Server Management

;; Define a global variable to store our server instance
(defvar *acceptor* nil
  "Global variable to hold the Hunchentoot acceptor instance.")

(defun get-server-config ()
  "Extract server configuration from the global config."
  (getf *config* :server))

(defun create-server-instance (port address)
  "Create a new Hunchentoot server instance with the specified configuration."
  (let ((server-config (get-server-config)))
    (make-instance 'hunchentoot:easy-acceptor 
                   :port port
                   :address address
                   :document-root (getf server-config :document-root #p"./static/")
                   :access-log-destination (getf server-config :access-log-destination nil)
                   :message-log-destination (getf server-config :message-log-destination nil))))

(defun start-server (&optional port)
  "Start the web server on the specified port or use the configured port.
   If the server is already running, it will be stopped first."
  (let* ((server-config (get-server-config))
         (server-port (or port (getf server-config :port)))
         (server-address (getf server-config :address)))
    
    (log-info "Starting the SBCL web service on port ~A..." server-port)
    
    ;; Stop the server if it's already running
    (when *acceptor*
      (log-info "Server is already running. Stopping it first...")
      (stop-server))
    
    ;; Create and start a new server instance with configuration
    (handler-case
        (progn
          (setf *acceptor* (create-server-instance server-port server-address))
          (hunchentoot:start *acceptor*)
          (log-info "Server started successfully on port ~A!" server-port)
          (log-info "Visit http://~A:~A/ to see the application" 
                   (if (string= server-address "0.0.0.0") "localhost" server-address)
                   server-port)
          *acceptor*)
      (error (e)
        (log-error "Failed to start server: ~A" e)
        (setf *acceptor* nil)
        nil))))

(defun stop-server ()
  "Stop the web server if it's running."
  (if *acceptor*
      (handler-case
          (progn
            (log-info "Stopping the server...")
            (hunchentoot:stop *acceptor*)
            (setf *acceptor* nil)
            (log-info "Server stopped.")
            t)  ; Return T to indicate success
        (error (e)
          (log-error "Error stopping server: ~A" e)
          nil))  ; Return NIL to indicate failure
      (progn
        (log-warn "No server running, nothing to stop.")
        t)))  ; Return T since there's no error
