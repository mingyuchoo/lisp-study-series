(defpackage :sbcl-web-service.web
  (:documentation "Web routes and handlers for the SBCL web service.")
  (:use :cl :sbcl-web-service.utils)
  (:export :*web-routes*
           :web-routes->dispatchers
           :route->dispatcher
           :register-web-routes
           :root-page
           :handle-404
           :static-file-path))

(in-package :sbcl-web-service.web)
