;; This is a startup script to help load the web service

;; First, load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed. Please install Quicklisp first.")))

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

;; Print a helpful message
(format t "~%Web service is running. Access it at http://localhost:8080/~%")
(format t "Available endpoints:~%")
(format t "  / - Hello World~%")
(format t "  /api/example - JSON API example~%")
(format t "  /health - Health check endpoint~%")
(format t "~%To stop the server, call (sbcl-web-service:stop-server)~%")
