(in-package :sbcl-web-service.web)

;;;; Web Route Definitions

(defun static-file-path (filename)
  "Create a path to a static file relative to the application root."
  (merge-pathnames (format nil "static/~A" filename) (truename ".")))

;; Root handler - Serve index.html
(hunchentoot:define-easy-handler (root-page :uri "/") ()
  "Serve the main index.html page."
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file (static-file-path "index.html")))

;;; Error Handlers

;; Define a handler for 404 errors (not a route handler)
(defun handle-404 ()
  "Handle 404 Not Found errors by serving a custom error page."
  (setf (hunchentoot:return-code*) 404)
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file (static-file-path "error.html")))

;;;; Web Route Registration

;; Register all web routes in the dispatch table
(defun register-web-routes ()
  "Register all web routes in the Hunchentoot dispatch table."
  ;; Static content routes
  (push (hunchentoot:create-prefix-dispatcher "/" 'root-page)
        hunchentoot:*dispatch-table*)
  
  ;; Error handlers - must be last to catch unmatched routes
  (push (hunchentoot:create-regex-dispatcher "^/(?!api/|$).*" 'handle-404)
        hunchentoot:*dispatch-table*))
