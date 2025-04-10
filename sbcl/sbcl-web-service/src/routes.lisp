(in-package :sbcl-web-service)

;;;; Route Definitions

;; Clear the dispatch table to start fresh
(setf hunchentoot:*dispatch-table* nil)

;;; Static Content Routes

(defun static-file-path (filename)
  "Create a path to a static file relative to the application root."
  (merge-pathnames (format nil "static/~A" filename) (truename ".")))

;; Root handler - Serve index.html
(hunchentoot:define-easy-handler (root-page :uri "/") ()
  "Serve the main index.html page."
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file (static-file-path "index.html")))

;;; API Routes

;; API example endpoint
(hunchentoot:define-easy-handler (api-example :uri "/api/example") ()
  "Return a sample JSON response for API testing."
  (json-response
   '((:status . "success")
     (:message . "API is working")
     (:data . ((:id . 1) (:name . "Example"))))))

;; Health check endpoint
(hunchentoot:define-easy-handler (health-check :uri "/api/health") ()
  "Return server health status information."
  (json-response
   `((:status . "ok")
     (:version . "1.0.0")
     (:timestamp . ,(get-timestamp)))))

;;; Error Handlers

;; Define a handler for 404 errors (not a route handler)
(defun handle-404 ()
  "Handle 404 Not Found errors by serving a custom error page."
  (setf (hunchentoot:return-code*) 404)
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file (static-file-path "error.html")))

;;;; Route Registration

;; Register all routes in the dispatch table
(defun register-routes ()
  "Register all application routes in the Hunchentoot dispatch table."
  ;; Static content routes
  (push (hunchentoot:create-prefix-dispatcher "/" 'root-page)
        hunchentoot:*dispatch-table*)
  
  ;; API routes
  (push (hunchentoot:create-prefix-dispatcher "/api/example" 'api-example)
        hunchentoot:*dispatch-table*)
  
  (push (hunchentoot:create-prefix-dispatcher "/api/health" 'health-check)
        hunchentoot:*dispatch-table*)
  
  ;; Error handlers - must be last to catch unmatched routes
  (push (hunchentoot:create-regex-dispatcher "^/(?!api/|$).*" 'handle-404)
        hunchentoot:*dispatch-table*))

;; Register all routes
(register-routes)
