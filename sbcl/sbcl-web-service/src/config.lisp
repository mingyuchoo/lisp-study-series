(in-package :sbcl-web-service)

;; Function to read environment variables from config.env file
(defun read-env-file (file-path)
  (let ((env-vars (make-hash-table :test #'equal)))
    (with-open-file (stream file-path :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil nil)
              while line
              do (let* ((trimmed-line (string-trim '(#\Space #\Tab) line))
                        (pos (position #\= trimmed-line)))
                   (when (and pos (> pos 0))
                     (let ((key (string-trim '(#\Space #\Tab) (subseq trimmed-line 0 pos)))
                           (value (string-trim '(#\Space #\Tab) (subseq trimmed-line (1+ pos)))))
                       (setf (gethash key env-vars) value)))))))
    env-vars))

;; Function to get environment variable with fallback
(defun get-env-var (env-vars key &optional default)
  (or (gethash key env-vars) default))

;; Read environment variables from config.env file
(defparameter *env-vars* (read-env-file #p"config.env"))

;; Configuration parameters
(defparameter *config* 
  `(:server (:port ,(parse-integer (get-env-var *env-vars* "PORT" "8080"))
             :address ,(get-env-var *env-vars* "IP" "127.0.0.1")
             :document-root #p"./static/"
             :access-log-destination nil
             :message-log-destination nil)))
