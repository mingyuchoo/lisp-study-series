#!/usr/bin/env -S sbcl --script

;;;; simple-server.lisp - Script to start the Common Lisp web application

(format t "~%Starting Common Lisp Web Application...~%")

;; Attempt to load QuickLisp
(format t "~%Attempting to load Quicklisp...~%")
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (truename "."))))
  (if (probe-file quicklisp-init)
      (handler-case
          (load quicklisp-init)
        (error (e)
          (format t "~%Error loading Quicklisp: ~A~%" e)
          (format t "~%Continuing with fallback server.~%")))
      (format t "~%Quicklisp not found, continuing with fallback server.~%")))

;; Add the current directory to ASDF's search path
(format t "~%Setting up ASDF...~%")
(push (truename ".") asdf:*central-registry*)

;; Try to load our application system with Hunchentoot
(format t "~%Attempting to load application with Hunchentoot...~%")
(handler-case
    (progn
      (require :hunchentoot)
      (format t "~%Hunchentoot loaded successfully.~%")
      (load "app.lisp")
      (format t "~%Main application loaded.~%"))
  (error (e)
    (format t "~%Error loading Hunchentoot-based application: ~A~%" e)
    (format t "~%Falling back to simple socket-based server...~%")
    (load "fallback-server.lisp")))

;; Print startup banner
(in-package :cl-webapp)
(print-startup-banner)

;; Start the web server
(format t "~%Starting server...~%")
(start-server)

;; Keep the script running
(format t "~%Server is running. Press Ctrl+C to stop.~%")
(loop
  (sleep 60))
