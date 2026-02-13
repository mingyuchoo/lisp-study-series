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

;;;; Web Route Table (declarative data)

(defparameter *web-routes*
  '((:regex   "^/$"                  . root-page)
    (:regex   "^/(?!api/|$).*"       . handle-404))
  "Declarative web route definitions as an alist of (type pattern . handler).
   Root uses exact regex match to avoid catching all URLs.
   404 regex acts as fallback for non-API, non-root paths.")

;;;; Web Route Registration

(defun route->dispatcher (route)
  "Convert a single route definition to a Hunchentoot dispatcher.
   Pure function: dispatches on route type (:prefix or :regex).
   ROUTE format: (:type pattern . handler)"
  (let ((type (car route))
        (pattern (cadr route))
        (handler (cddr route)))
    (ecase type
      (:prefix (hunchentoot:create-prefix-dispatcher pattern handler))
      (:regex  (hunchentoot:create-regex-dispatcher pattern handler)))))

(defun web-routes->dispatchers (routes)
  "Transform a list of web route definitions into Hunchentoot dispatchers.
   Pure function: returns a list without modifying global state."
  (mapcar #'route->dispatcher routes))

(defun register-web-routes ()
  "Register all web routes in the Hunchentoot dispatch table."
  (setf hunchentoot:*dispatch-table*
        (append (web-routes->dispatchers *web-routes*)
                hunchentoot:*dispatch-table*)))
