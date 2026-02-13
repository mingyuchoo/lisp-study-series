(defpackage :sbcl-web-service.tests
  (:use :cl :fiveam)
  (:import-from :drakma :http-request)
  (:import-from :sbcl-web-service :start-server :stop-server :*config* :*acceptor*)
  (:export :run-tests))

(in-package :sbcl-web-service.tests)

;;;; Test Infrastructure

(defparameter *test-port* 8088
  "Default port for test server instances.")

(defun call-with-test-server (test-fn &optional (port *test-port*))
  "Higher-order function: start a server, call TEST-FN with the port,
   and guarantee cleanup via unwind-protect."
  (unwind-protect
      (progn
        (start-server port)
        (funcall test-fn port))
    (stop-server)))

(defun test-url (port path)
  "Build a test URL from port and path. Pure function."
  (format nil "http://localhost:~A~A" port path))

;;;; Test Suite

(def-suite sbcl-web-service-tests
  :description "Tests for the SBCL web service")

(in-suite sbcl-web-service-tests)

(test server-startup
  "Test that the server starts up correctly"
  (call-with-test-server
   (lambda (port)
     (declare (ignore port))
     (is-true *acceptor*))))

(test hello-world-endpoint
  "Test that the root endpoint returns 'Hello, World!'"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/") :force-binary nil)
       (is (= status 200))
       (is (string= "Hello, World!" body))))))

(test api-endpoint
  "Test that the API endpoint returns valid JSON"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/api/example") :force-binary nil)
       (is (= status 200))
       (let ((json-response (json:decode-json-from-string body)))
         (is (equal "success" (cdr (assoc :status json-response))))
         (is (equal "API is working" (cdr (assoc :message json-response))))
         (is (assoc :data json-response)))))))

(test health-endpoint
  "Test that the health endpoint returns valid JSON"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/api/health") :force-binary nil)
       (is (= status 200))
       (let ((json-response (json:decode-json-from-string body)))
         (is (equal "ok" (cdr (assoc :status json-response))))
         (is (assoc :version json-response))
         (is (assoc :timestamp json-response)))))))

(test not-found-handling
  "Test that unknown routes return 404"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/unknown-route") :force-binary nil)
       (is (= status 404))
       (is (search "Page Not Found" body))))))

;;;; Test Runners

(defun run-tests ()
  (format t "Running tests for the SBCL web service...~%")
  (run! 'sbcl-web-service-tests)
  (format t "Tests completed.~%"))

(defun run-tests-interactive ()
  (format t "Running tests for the SBCL web service interactively...~%")
  (run! 'sbcl-web-service-tests)
  (explain! (get-test 'sbcl-web-service-tests))
  (format t "Interactive tests completed.~%"))
