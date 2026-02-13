(defpackage :sbcl-web-service.core
  (:documentation "Core functionality for the SBCL web service.")
  (:use :cl :split-sequence)
  (:export ;; Configuration (pure)
           :*config*
           :*env-vars*
           :parse-env-line
           :get-env-var
           :read-env-file
           :parse-env-var-integer
           ;; Server - pure functions
           :extract-server-config
           :resolve-server-params
           :make-acceptor
           :display-address
           ;; Server - effect functions
           :start-acceptor
           :stop-acceptor
           ;; Server - stateful shell
           :start-server
           :stop-server
           :*acceptor*))

(in-package :sbcl-web-service.core)

;;;; Environment Configuration

(defun parse-env-line (line)
  "Parse a single KEY=VALUE line into a cons cell, or NIL if invalid.
   Pure function: no side effects."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (pos (position #\= trimmed)))
    (when (and (plusp (length trimmed))
               (not (char= (char trimmed 0) #\#))
               pos (> pos 0))
      (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
            (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos)))))))

(defun read-env-file (file-path)
  "Read environment variables from a file in KEY=VALUE format.
   Returns an immutable alist of (key . value) string pairs."
  (handler-case
      (with-open-file (stream file-path :if-does-not-exist nil)
        (when stream
          (loop for line = (read-line stream nil nil)
                while line
                for pair = (parse-env-line line)
                when pair collect pair)))
    (error (e)
      (format *error-output* "Error reading environment file ~A: ~A~%" file-path e)
      nil)))

(defun get-env-var (env-vars key &optional default)
  "Look up an environment variable from an alist.
   Pure function: returns the value or default without side effects."
  (let ((pair (assoc key env-vars :test #'equal)))
    (if pair (cdr pair) default)))

(defun parse-env-var-integer (env-vars key &optional default)
  "Parse an environment variable as an integer with error handling."
  (handler-case
      (parse-integer (get-env-var env-vars key (princ-to-string default)))
    (error (e)
      (format *error-output* "Error parsing ~A as integer: ~A. Using default ~A.~%" key e default)
      default)))

;;;; Application Configuration

;; Read environment variables from config.env file
(defparameter *env-vars* (read-env-file #p"config.env"))

;; Server configuration parameters
(defparameter *config* 
  `(:server (:port ,(parse-env-var-integer *env-vars* "PORT" 8080)
             :address ,(get-env-var *env-vars* "IP" "127.0.0.1")
             :document-root #p"./static/"
             :access-log-destination nil
             :message-log-destination nil)))
