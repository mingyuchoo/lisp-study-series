(in-package :sbcl-web-service)

;; Clear the dispatch table to start fresh
(setf hunchentoot:*dispatch-table* nil)

;; Root handler - Serve index.html
(hunchentoot:define-easy-handler (root-page :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file
   (merge-pathnames "static/index.html" (truename "."))))

;; API example endpoint
(hunchentoot:define-easy-handler (api-example :uri "/api/example") ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   '((:status . "success")
     (:message . "API is working")
     (:data . ((:id . 1) (:name . "Example"))))))

;; Health check endpoint
(hunchentoot:define-easy-handler (health-check :uri "/api/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   `((:status . "ok")
     (:version . "1.0.0")
     (:timestamp . ,(get-timestamp)))))

;; Define a handler for 404 errors (not a route handler)
(defun handle-404 ()
  (setf (hunchentoot:return-code*) 404)
  (setf (hunchentoot:content-type*) "text/html")
  (hunchentoot:handle-static-file
   (merge-pathnames "static/error.html" (truename "."))))

;; Add all our defined handlers to the dispatch table
(push (hunchentoot:create-prefix-dispatcher "/" 'root-page)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-prefix-dispatcher "/api/example" 'api-example)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-prefix-dispatcher "/api/health" 'health-check)
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-regex-dispatcher "^/(?!api/|$).*" 'handle-404)
      hunchentoot:*dispatch-table*)
