(defpackage :sbcl-web-service.tests
  (:use :cl :fiveam)
  (:import-from :drakma :http-request)
  (:import-from :sbcl-web-service :start-server :stop-server :*config* :*acceptor*)
  (:export :run-tests))

(in-package :sbcl-web-service.tests)

;; Define a test suite for our web service
(def-suite sbcl-web-service-tests
  :description "Tests for the SBCL web service")

(in-suite sbcl-web-service-tests)

;; Test for server startup
(test server-startup
  "Test that the server starts up correctly"
  (let ((port 8088))
    ;; Start the server on a test port
    (start-server port)
    ;; Check that the server is running
    (is-true *acceptor*)
    ;; Stop the server
    (stop-server)))

;; Test for the root endpoint
(test hello-world-endpoint
  "Test that the root endpoint returns 'Hello, World!'"
  (let ((port 8088))
    ;; Start the server on a test port
    (start-server port)
    ;; Test the root endpoint
    (multiple-value-bind (body status)
        (http-request (format nil "http://localhost:~A/" port) :force-binary nil)
      ;; Check that we get a 200 OK response
      (is (= status 200))
      ;; Check that the body contains the expected content
      (is (string= "Hello, World!" body)))
    ;; Stop the server
    (stop-server)))

;; Test for API endpoint
(test api-endpoint
  "Test that the API endpoint returns valid JSON"
  (let ((port 8088))
    ;; Start the server on a test port
    (start-server port)
    ;; Test the API endpoint
    (multiple-value-bind (body status)
        (http-request (format nil "http://localhost:~A/api/example" port) :force-binary nil)
      ;; Check that we get a 200 OK response
      (is (= status 200))
      ;; Check that the body is valid JSON and contains expected fields
      (let ((json-response (json:decode-json-from-string body)))
        (is (equal "success" (cdr (assoc :status json-response))))
        (is (equal "API is working" (cdr (assoc :message json-response))))
        (is (assoc :data json-response))))
    ;; Stop the server
    (stop-server)))

;; Test for health endpoint
(test health-endpoint
  "Test that the health endpoint returns valid JSON"
  (let ((port 8088))
    ;; Start the server on a test port
    (start-server port)
    ;; Test the health endpoint
    (multiple-value-bind (body status)
        (http-request (format nil "http://localhost:~A/api/health" port) :force-binary nil)
      ;; Check that we get a 200 OK response
      (is (= status 200))
      ;; Check that the body is valid JSON and contains expected fields
      (let ((json-response (json:decode-json-from-string body)))
        (is (equal "ok" (cdr (assoc :status json-response))))
        (is (assoc :version json-response))
        (is (assoc :timestamp json-response))))
    ;; Stop the server
    (stop-server)))

;; Test for 404 handling
(test not-found-handling
  "Test that unknown routes return 404"
  (let ((port 8088))
    ;; Start the server on a test port
    (start-server port)
    ;; Test an unknown route
    (multiple-value-bind (body status)
        (http-request (format nil "http://localhost:~A/unknown-route" port) :force-binary nil)
      ;; Check that we get a 404 response
      (is (= status 404))
      ;; Check that the body contains the 404 error page content
      (is (search "Page Not Found" body)))
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