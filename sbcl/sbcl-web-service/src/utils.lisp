(in-package :sbcl-web-service)

;;;; Date and Time Utilities

(defun get-timestamp ()
  "Return the current timestamp in ISO 8601 format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

;;;; Logging System

(defparameter *log-levels* '(:debug :info :warn :error :fatal)
  "Available logging levels in order of severity.")

(defparameter *current-log-level* :info
  "Current log level threshold. Messages below this level won't be logged.")

(defun log-level-enabled-p (level)
  "Check if the specified log level is enabled based on current threshold."
  (let ((current-pos (position *current-log-level* *log-levels*))
        (level-pos (position level *log-levels*)))
    (and current-pos level-pos (>= level-pos current-pos))))

(defun log-message (level message &rest args)
  "Log a message with the specified level if that level is enabled."
  (when (log-level-enabled-p level)
    (let ((timestamp (get-timestamp))
          (formatted-message (apply #'format nil message args)))
      (format t "[~A] [~A] ~A~%" timestamp (string-upcase (symbol-name level)) formatted-message))))

;; Convenience logging functions
(defun log-debug (message &rest args)
  "Log a debug message."
  (apply #'log-message :debug message args))

(defun log-info (message &rest args)
  "Log an info message."
  (apply #'log-message :info message args))

(defun log-warn (message &rest args)
  "Log a warning message."
  (apply #'log-message :warn message args))

(defun log-error (message &rest args)
  "Log an error message."
  (apply #'log-message :error message args))

(defun log-fatal (message &rest args)
  "Log a fatal message."
  (apply #'log-message :fatal message args))

;;;; JSON Utilities

(defun parse-json-safely (json-string)
  "Safely parse a JSON string, returning NIL on error."
  (handler-case (json:decode-json-from-string json-string)
    (error (e)
      (log-error "Failed to parse JSON: ~A" e)
      nil)))

(defun json-response (data &optional (status 200))
  "Create a JSON response with the given data and status code."
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (json:encode-json-to-string data))
