;; Load Quicklisp first - this must be done before anything else
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed. Please install Quicklisp first.")))

;; Load required packages
(ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities))

;; This is the main entry point file that loads all components
;; The actual package definition is in package.lisp

;; Load the components in the correct order
;; 1. Package definition
;; 2. Configuration
;; 3. Utilities
;; 4. Server management
;; 5. Routes/controllers

;; Note: In a real ASDF system, these would be loaded automatically
;; based on the system definition, but we're loading them manually here
;; for clarity

;; Load the package definition first
(load "src/package.lisp")

;; Load configuration
(load "src/config.lisp")

;; Load server management
(load "src/server.lisp")

;; Load routes/controllers
(load "src/routes.lisp")

;; Main function to start the application
(defun sbcl-web-service:main ()
  (format t "~%Starting SBCL Web Service Application~%")
  (sbcl-web-service:start-server))