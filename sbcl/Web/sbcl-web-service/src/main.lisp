(defpackage :sbcl-web-service
  (:documentation "A web service built with Steel Bank Common Lisp.")
  (:use :cl :split-sequence
        :sbcl-web-service.core
        :sbcl-web-service.utils
        :sbcl-web-service.api
        :sbcl-web-service.web)
  (:export ;; Main entry point
           :main
           :load-dependencies
           :build-dispatch-table
           :initialize-routes
           ;; Re-export core functionality
           :start-server
           :stop-server
           :*config*
           :*acceptor*
           :get-env-var
           :read-env-file))

(in-package :sbcl-web-service)

;;;; Main Application Entry Point

(defun load-dependencies (&optional (deps '(:hunchentoot :cl-json :alexandria
                                            :cl-ppcre :cl-utilities :split-sequence)))
  "Load all required dependencies for the application.
   Accepts an optional list of dependency names for testability."
  (handler-case
      (progn
        (ql:quickload deps :silent t)
        t)  ; Return T on success
    (error (e)
      (format *error-output* "Error loading dependencies: ~A~%" e)
      nil)))  ; Return NIL on failure

(defun build-dispatch-table ()
  "Build a complete dispatch table by composing all route modules.
   API routes come first for specific path matching, then web routes.
   Pure function: returns a new dispatch table list without side effects."
  (append (sbcl-web-service.api:api-routes->dispatchers
           sbcl-web-service.api:*api-routes*)
          (sbcl-web-service.web:web-routes->dispatchers
           sbcl-web-service.web:*web-routes*)))

(defun initialize-routes ()
  "Install the composed dispatch table into Hunchentoot.
   Single point of side effect for route registration."
  (setf hunchentoot:*dispatch-table* (build-dispatch-table))
  t)

;; Main function to start the application
(defun main ()
  "Start the SBCL Web Service application.
   This is the main entry point for the application."
  (log-info "Starting SBCL Web Service Application")

  ;; Ensure dependencies are loaded
  (unless (load-dependencies)
    (log-error "Failed to load dependencies. Aborting startup.")
    (return-from main nil))

  ;; Initialize routes
  (initialize-routes)

  ;; Start the server
  (let ((server (start-server)))
    (if server
        (log-info "Application started successfully!")
        (log-error "Failed to start the application."))
    server))
