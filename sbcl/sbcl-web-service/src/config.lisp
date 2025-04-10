(in-package :sbcl-web-service)

;;;; Environment Configuration

(defun read-env-file (file-path)
  "Read environment variables from a file in KEY=VALUE format.
   Returns a hash table with keys and values as strings."
  (let ((env-vars (make-hash-table :test #'equal)))
    (handler-case
        (with-open-file (stream file-path :if-does-not-exist nil)
          (when stream
            (loop for line = (read-line stream nil nil)
                  while line
                  do (let* ((trimmed-line (string-trim '(#\Space #\Tab) line))
                            (pos (position #\= trimmed-line)))
                       (when (and (not (string= "" trimmed-line))
                                  (not (char= (char trimmed-line 0) #\#))
                                  pos (> pos 0))
                         (let ((key (string-trim '(#\Space #\Tab) (subseq trimmed-line 0 pos)))
                               (value (string-trim '(#\Space #\Tab) (subseq trimmed-line (1+ pos)))))
                           (setf (gethash key env-vars) value)))))))
      (error (e)
        (format *error-output* "Error reading environment file ~A: ~A~%" file-path e)))
    env-vars))

(defun get-env-var (env-vars key &optional default)
  "Get environment variable value from the provided hash table.
   If the key doesn't exist, returns the default value."
  (or (gethash key env-vars) default))

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
