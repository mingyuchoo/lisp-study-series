(defpackage :sbcl-web-service.api
  (:documentation "API endpoints for the SBCL web service.")
  (:use :cl :sbcl-web-service.utils)
  (:export :register-api-routes
           :api-example
           :health-check))

(in-package :sbcl-web-service.api)
