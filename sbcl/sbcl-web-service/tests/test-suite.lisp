(defpackage :sbcl-web-service.tests
  (:use :cl :fiveam)
  (:import-from :drakma :http-request)
  (:import-from :sbcl-web-service :start-server :stop-server)
  (:export :run-tests))

(in-package :sbcl-web-service.tests)

;; Define a test suite for our web service
(def-suite sbcl-web-service-tests
  :description "Tests for the SBCL web service")

(in-suite sbcl-web-service-tests)

;; Test for server startup
(test server-startup
  "Test that the server starts up correctly"
  (let ((server nil))
    (finishes
      (setf server (start-server 8088)))
    (is-true server)
    (finishes
      (stop-server))))

;; Test for hello world endpoint
(test hello-world-endpoint
  "Test that the root endpoint returns 'Hello, World!'"
  (let ((server nil)
        (port 8088))
    ;; Start the server on a test port
    (setf server (start-server port))
    ;; Make sure the server is running
    (is-true server)
    ;; Test the root endpoint
    (multiple-value-bind (body status)
        (http-request (format nil "http://localhost:~A/" port))
      ;; Check that we get a 200 OK response
      (is (= status 200))
      ;; Check that the body contains our expected message
      (is (string= "Hello, World!" (flexi-streams:octets-to-string body))))
    ;; Stop the server
    (stop-server)))

;; Function to run all tests
(defun run-tests ()
  (format t "Running tests for the SBCL web service...~%")
  (run! 'sbcl-web-service-tests)
  (format t "Tests completed.~%"))

;; Function to run tests interactively
(defun run-tests-interactive ()
  (format t "Running tests for the SBCL web service interactively...~%")
  (run! 'sbcl-web-service-tests)
  (explain! (get-test 'sbcl-web-service-tests))
  (format t "Interactive tests completed.~%"))