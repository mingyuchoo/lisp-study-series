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

;;;; API Route Registration

;; Register all API routes in the dispatch table
(defun register-api-routes ()
  "Register all API routes in the Hunchentoot dispatch table."
  ;; API routes
  (push (hunchentoot:create-prefix-dispatcher "/api/example" 'api-example)
        hunchentoot:*dispatch-table*)
  
  (push (hunchentoot:create-prefix-dispatcher "/api/health" 'health-check)
        hunchentoot:*dispatch-table*))
