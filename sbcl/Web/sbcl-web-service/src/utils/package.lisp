(defpackage :sbcl-web-service.utils
  (:documentation "Utility functions for the SBCL web service.")
  (:use :cl :split-sequence)
  (:export ;; Logging
           :log-debug
           :log-info
           :log-warn
           :log-error
           :log-fatal
           ;; Time utilities
           :get-timestamp
           ;; JSON utilities
           :encode-json-body
           :parse-json-safely
           :json-response))

(in-package :sbcl-web-service.utils)
