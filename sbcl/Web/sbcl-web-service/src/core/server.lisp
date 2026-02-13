(in-package :sbcl-web-service.core)

;;;; Server Management
;;;; Organized as: Pure Functions → Effect Functions → Stateful Shell

;;; --- Pure Functions (no side effects, no global state access) ---

(defun extract-server-config (config)
  "Extract the server sub-configuration from a config plist.
   Pure function: takes config explicitly, does not read globals."
  (getf config :server))

(defun resolve-server-params (config &optional override-port)
  "Resolve server parameters from config, with optional port override.
   Returns a plist (:port port :address address :document-root root ...).
   Pure function."
  (let ((server-config (extract-server-config config)))
    (list :port (or override-port (getf server-config :port))
          :address (getf server-config :address)
          :document-root (getf server-config :document-root #p"./static/")
          :access-log-destination (getf server-config :access-log-destination nil)
          :message-log-destination (getf server-config :message-log-destination nil))))

(defun make-acceptor (params)
  "Create a new Hunchentoot acceptor from resolved server params.
   Pure function: allocates an object but does not start it."
  (make-instance 'hunchentoot:easy-acceptor
                 :port (getf params :port)
                 :address (getf params :address)
                 :document-root (getf params :document-root)
                 :access-log-destination (getf params :access-log-destination)
                 :message-log-destination (getf params :message-log-destination)))

(defun display-address (address)
  "Convert bind address to a human-friendly display address.
   Pure function."
  (if (string= address "0.0.0.0") "localhost" address))

;;; --- Effect Functions (minimal side effects, no global state mutation) ---

(defun start-acceptor (acceptor)
  "Start a Hunchentoot acceptor. Side effect: begins listening on the network.
   Returns the started acceptor."
  (hunchentoot:start acceptor)
  acceptor)

(defun stop-acceptor (acceptor)
  "Stop a Hunchentoot acceptor. Side effect: stops listening on the network.
   Returns T on success, NIL on failure."
  (handler-case
      (progn
        (hunchentoot:stop acceptor)
        t)
    (error (e)
      (sbcl-web-service.utils:log-error "Error stopping server: ~A" e)
      nil)))

;;; --- Stateful Shell (manages *acceptor* global, composes pure + effect) ---

(defvar *acceptor* nil
  "Global variable to hold the running Hunchentoot acceptor instance.")

(defun start-server (&optional port)
  "Start the web server. Composes pure config resolution, acceptor creation,
   and effectful server start. Manages global *acceptor* state."
  (let* ((params (resolve-server-params *config* port))
         (server-port (getf params :port))
         (server-address (getf params :address)))

    (sbcl-web-service.utils:log-info "Starting the SBCL web service on port ~A..." server-port)

    (when *acceptor*
      (sbcl-web-service.utils:log-info "Server is already running. Stopping it first...")
      (stop-server))

    (handler-case
        (let ((acceptor (start-acceptor (make-acceptor params))))
          (setf *acceptor* acceptor)
          (sbcl-web-service.utils:log-info "Server started successfully on port ~A!" server-port)
          (sbcl-web-service.utils:log-info "Visit http://~A:~A/ to see the application"
                   (display-address server-address) server-port)
          acceptor)
      (error (e)
        (sbcl-web-service.utils:log-error "Failed to start server: ~A" e)
        (setf *acceptor* nil)
        nil))))

(defun stop-server ()
  "Stop the web server if running. Manages global *acceptor* state."
  (if *acceptor*
      (progn
        (sbcl-web-service.utils:log-info "Stopping the server...")
        (let ((result (stop-acceptor *acceptor*)))
          (setf *acceptor* nil)
          (if result
              (progn (sbcl-web-service.utils:log-info "Server stopped.") t)
              nil)))
      (progn
        (sbcl-web-service.utils:log-warn "No server running, nothing to stop.")
        t)))
