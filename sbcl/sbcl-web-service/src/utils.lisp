(in-package :sbcl-web-service)

;; Utility function to get the current timestamp in ISO 8601 format
(defun get-timestamp ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

;; Utility function for logging
(defun log-message (level message &rest args)
  (let ((timestamp (get-timestamp))
        (formatted-message (apply #'format nil message args)))
    (format t "[~A] [~A] ~A~%" timestamp level formatted-message)))

;; Convenience logging functions
(defun log-info (message &rest args)
  (apply #'log-message "INFO" message args))

(defun log-error (message &rest args)
  (apply #'log-message "ERROR" message args))

;; Utility function to safely parse JSON
(defun parse-json-safely (json-string)
  (handler-case (json:decode-json-from-string json-string)
    (error (e)
      (log-error "Failed to parse JSON: ~A" e)
      nil)))
