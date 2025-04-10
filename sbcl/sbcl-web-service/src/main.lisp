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

;; Main function to start the application
(defun sbcl-web-service:main ()
  "Start the SBCL Web Service application.
   This is the main entry point for the application."
  (format t "~%Starting SBCL Web Service Application~%")
  
  ;; Ensure dependencies are loaded
  (unless (load-dependencies)
    (format t "Failed to load dependencies. Aborting startup.~%")
    (return-from sbcl-web-service:main nil))
  
  ;; Start the server
  (let ((server (sbcl-web-service:start-server)))
    (if server
        (format t "Application started successfully!~%")
        (format t "Failed to start the application.~%"))
    server))