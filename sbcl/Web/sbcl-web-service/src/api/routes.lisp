(defpackage :sbcl-web-service.api
  (:documentation "API endpoints for the SBCL web service.")
  (:use :cl :sbcl-web-service.utils)
  (:export :*api-routes*
           :api-routes->dispatchers
           :register-api-routes
           :api-example
           :health-check))

(in-package :sbcl-web-service.api)

;;;; API Route Definitions

;; API example endpoint
(hunchentoot:define-easy-handler (api-example :uri "/api/example") ()
  "Return a sample JSON response for API testing."
  (sbcl-web-service.utils:json-response
   '((:status . "success")
     (:message . "API is working")
     (:data . ((:id . 1) (:name . "Example"))))))

;; Health check endpoint
(hunchentoot:define-easy-handler (health-check :uri "/api/health") ()
  "Return server health status information."
  (sbcl-web-service.utils:json-response
   `((:status . "ok")
     (:version . "1.0.0")
     (:timestamp . ,(sbcl-web-service.utils:get-timestamp)))))

;;;; API Route Table (declarative data)

(defparameter *api-routes*
  '(("/api/example" . api-example)
    ("/api/health"  . health-check))
  "Declarative API route definitions as an alist of (prefix . handler).")

;;;; API Route Registration

(defun api-routes->dispatchers (routes)
  "Transform an alist of route definitions into Hunchentoot dispatchers.
   Pure function: returns a list without modifying global state."
  (mapcar (lambda (route)
            (hunchentoot:create-prefix-dispatcher (car route) (cdr route)))
          routes))

(defun register-api-routes ()
  "Register all API routes in the Hunchentoot dispatch table."
  (setf hunchentoot:*dispatch-table*
        (append (api-routes->dispatchers *api-routes*)
                hunchentoot:*dispatch-table*)))
