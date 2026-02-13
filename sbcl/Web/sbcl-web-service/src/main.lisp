(in-package :sbcl-web-service)

;;;; Main Application Entry Point

;; Load Quicklisp first - this must be done before anything else
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed. Please install Quicklisp first.")))

;; Define a function to load required dependencies
(defun load-dependencies ()
  "Load all required dependencies for the application."
  (handler-case
      (progn
        (ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities :split-sequence) :silent t)
        t)  ; Return T on success
    (error (e)
      (format *error-output* "Error loading dependencies: ~A~%" e)
      nil)))  ; Return NIL on failure

(defun build-dispatch-table ()
  "Build a complete dispatch table by composing all route modules.
   Pure function: returns a new dispatch table list without side effects."
  (append (sbcl-web-service.web:web-routes->dispatchers
           sbcl-web-service.web:*web-routes*)
          (sbcl-web-service.api:api-routes->dispatchers
           sbcl-web-service.api:*api-routes*)))

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