(defpackage :sbcl-web-service.tests
  (:use :cl :fiveam)
  (:import-from :drakma :http-request)
  ;; Main package
  (:import-from :sbcl-web-service
                :start-server :stop-server :*config* :*acceptor*
                :build-dispatch-table :initialize-routes)
  ;; Utils
  (:import-from :sbcl-web-service.utils
                :get-timestamp
                :*log-levels* :*current-log-level*
                :log-level-enabled-p :log-message
                :log-debug :log-info :log-warn :log-error :log-fatal
                :parse-json-safely :encode-json-body)
  ;; Core - config
  (:import-from :sbcl-web-service.core
                :parse-env-line :read-env-file :get-env-var
                :parse-env-var-integer :*env-vars*
                :extract-server-config :resolve-server-params
                :make-acceptor :display-address
                :start-acceptor :stop-acceptor)
  ;; Web routes
  (:import-from :sbcl-web-service.web
                :static-file-path :route->dispatcher :web-routes->dispatchers
                :*web-routes* :register-web-routes)
  ;; API routes
  (:import-from :sbcl-web-service.api
                :api-routes->dispatchers :*api-routes* :register-api-routes)
  (:export :run-tests))

(in-package :sbcl-web-service.tests)

;;;; ============================================================
;;;; Test Infrastructure
;;;; ============================================================

(defparameter *test-port* 8088
  "Default port for test server instances.")

(defun call-with-test-server (test-fn &optional (port *test-port*))
  "Higher-order function: start a server, call TEST-FN with the port,
   and guarantee cleanup via unwind-protect."
  (unwind-protect
      (progn
        (initialize-routes)
        (start-server port)
        (funcall test-fn port))
    (stop-server)))

(defun test-url (port path)
  "Build a test URL from port and path. Pure function."
  (format nil "http://localhost:~A~A" port path))

(defun ensure-string (body)
  "Convert HTTP response body to string.
   Drakma returns byte arrays for non-text content types like application/json."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

;;;; ============================================================
;;;; Master Test Suite
;;;; ============================================================

(def-suite sbcl-web-service-tests
  :description "All tests for the SBCL web service")

;;;; ============================================================
;;;; Utils Module Tests
;;;; ============================================================

(def-suite utils-tests
  :description "Unit tests for sbcl-web-service.utils"
  :in sbcl-web-service-tests)

(in-suite utils-tests)

;;; --- Timestamp ---

(test get-timestamp-format
  "Test that get-timestamp returns ISO 8601 format (YYYY-MM-DDTHH:MM:SSZ)"
  (let ((ts (get-timestamp)))
    (is (stringp ts))
    (is (= 20 (length ts)))
    (is (char= #\T (char ts 10)))
    (is (char= #\Z (char ts 19)))
    (is (char= #\- (char ts 4)))
    (is (char= #\- (char ts 7)))
    (is (char= #\: (char ts 13)))
    (is (char= #\: (char ts 16)))))

(test get-timestamp-changes
  "Test that timestamps are consistent within same second"
  (let ((ts1 (get-timestamp))
        (ts2 (get-timestamp)))
    ;; Both calls within same test should produce valid timestamps
    (is (stringp ts1))
    (is (stringp ts2))))

;;; --- Log Level ---

(test log-level-enabled-p-at-info
  "Test log-level-enabled-p with info threshold"
  (let ((*current-log-level* :info))
    (is-false (log-level-enabled-p :debug))
    (is-true (log-level-enabled-p :info))
    (is-true (log-level-enabled-p :warn))
    (is-true (log-level-enabled-p :error))
    (is-true (log-level-enabled-p :fatal))))

(test log-level-enabled-p-at-debug
  "Test that all levels are enabled when threshold is debug"
  (let ((*current-log-level* :debug))
    (is-true (log-level-enabled-p :debug))
    (is-true (log-level-enabled-p :info))
    (is-true (log-level-enabled-p :warn))
    (is-true (log-level-enabled-p :error))
    (is-true (log-level-enabled-p :fatal))))

(test log-level-enabled-p-at-fatal
  "Test that only fatal is enabled when threshold is fatal"
  (let ((*current-log-level* :fatal))
    (is-false (log-level-enabled-p :debug))
    (is-false (log-level-enabled-p :info))
    (is-false (log-level-enabled-p :warn))
    (is-false (log-level-enabled-p :error))
    (is-true (log-level-enabled-p :fatal))))

(test log-level-enabled-p-invalid
  "Test that an invalid log level returns nil"
  (is-false (log-level-enabled-p :nonexistent)))

;;; --- Log Message ---

(test log-message-outputs-when-enabled
  "Test that log-message produces formatted output when level is enabled"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-message :info "test message ~A" 42))))
      (is (search "INFO" output))
      (is (search "test message 42" output)))))

(test log-message-suppressed-when-disabled
  "Test that log-message produces no output when level is disabled"
  (let ((*current-log-level* :error))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-message :info "should not appear"))))
      (is (string= "" output)))))

(test log-message-includes-timestamp
  "Test that log-message output includes a timestamp"
  (let ((*current-log-level* :debug))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-message :debug "timestamp test"))))
      ;; Timestamp format: [YYYY-MM-DDTHH:MM:SSZ]
      (is (search "T" output))
      (is (search "Z]" output)))))

;;; --- Convenience Logging Functions ---

(test log-debug-function
  "Test log-debug produces DEBUG output"
  (let ((*current-log-level* :debug))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-debug "debug ~A" "msg"))))
      (is (search "DEBUG" output))
      (is (search "debug msg" output)))))

(test log-info-function
  "Test log-info produces INFO output"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-info "info ~A" "msg"))))
      (is (search "INFO" output))
      (is (search "info msg" output)))))

(test log-warn-function
  "Test log-warn produces WARN output"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-warn "warn ~A" "msg"))))
      (is (search "WARN" output))
      (is (search "warn msg" output)))))

(test log-error-function
  "Test log-error produces ERROR output"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-error "error ~A" "msg"))))
      (is (search "ERROR" output))
      (is (search "error msg" output)))))

(test log-fatal-function
  "Test log-fatal produces FATAL output"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-fatal "fatal ~A" "msg"))))
      (is (search "FATAL" output))
      (is (search "fatal msg" output)))))

(test log-debug-suppressed-at-info
  "Test that log-debug is suppressed when threshold is info"
  (let ((*current-log-level* :info))
    (let ((output (with-output-to-string (*standard-output*)
                    (log-debug "should not appear"))))
      (is (string= "" output)))))

;;; --- JSON Utilities ---

(test parse-json-safely-valid
  "Test parsing valid JSON"
  (let ((result (parse-json-safely "{\"key\": \"value\"}")))
    (is (listp result))
    (is (equal "value" (cdr (assoc :key result))))))

(test parse-json-safely-nested
  "Test parsing nested JSON"
  (let ((result (parse-json-safely "{\"outer\": {\"inner\": 42}}")))
    (is (listp result))
    (let ((outer (cdr (assoc :outer result))))
      (is (= 42 (cdr (assoc :inner outer)))))))

(test parse-json-safely-invalid
  "Test parsing invalid JSON returns nil"
  (let ((*current-log-level* :fatal))
    (is-false (parse-json-safely "not valid json {"))))

(test parse-json-safely-empty
  "Test parsing empty string returns nil"
  (let ((*current-log-level* :fatal))
    (is-false (parse-json-safely ""))))

(test encode-json-body-alist
  "Test encoding an alist to JSON"
  (let ((result (encode-json-body '((:status . "ok") (:count . 42)))))
    (is (stringp result))
    (is (search "status" result))
    (is (search "ok" result))
    (is (search "42" result))))

(test encode-json-body-simple
  "Test encoding a simple value to JSON"
  (let ((result (encode-json-body '((:message . "hello")))))
    (is (stringp result))
    (is (search "hello" result))))

;;;; ============================================================
;;;; Config Module Tests
;;;; ============================================================

(def-suite config-tests
  :description "Unit tests for sbcl-web-service.core config"
  :in sbcl-web-service-tests)

(in-suite config-tests)

;;; --- parse-env-line ---

(test parse-env-line-valid
  "Test parsing a valid KEY=VALUE line"
  (let ((result (parse-env-line "PORT=8080")))
    (is (consp result))
    (is (equal "PORT" (car result)))
    (is (equal "8080" (cdr result)))))

(test parse-env-line-with-spaces
  "Test parsing a line with surrounding spaces"
  (let ((result (parse-env-line "  KEY = VALUE  ")))
    (is (consp result))
    (is (equal "KEY" (car result)))
    (is (equal "VALUE" (cdr result)))))

(test parse-env-line-comment
  "Test that comment lines return nil"
  (is-false (parse-env-line "# This is a comment")))

(test parse-env-line-empty
  "Test that empty lines return nil"
  (is-false (parse-env-line ""))
  (is-false (parse-env-line "   ")))

(test parse-env-line-no-equals
  "Test that lines without = return nil"
  (is-false (parse-env-line "NOEQUALS")))

(test parse-env-line-empty-value
  "Test parsing a line with empty value"
  (let ((result (parse-env-line "KEY=")))
    (is (consp result))
    (is (equal "KEY" (car result)))
    (is (equal "" (cdr result)))))

(test parse-env-line-equals-at-start
  "Test that = at the start returns nil (empty key)"
  (is-false (parse-env-line "=VALUE")))

(test parse-env-line-multiple-equals
  "Test that only the first = is used as separator"
  (let ((result (parse-env-line "KEY=VAL=UE")))
    (is (consp result))
    (is (equal "KEY" (car result)))
    (is (equal "VAL=UE" (cdr result)))))

;;; --- get-env-var ---

(test get-env-var-found
  "Test looking up an existing env var"
  (let ((vars '(("PORT" . "8080") ("IP" . "127.0.0.1"))))
    (is (equal "8080" (get-env-var vars "PORT")))
    (is (equal "127.0.0.1" (get-env-var vars "IP")))))

(test get-env-var-missing
  "Test looking up a missing env var returns default"
  (let ((vars '(("PORT" . "8080"))))
    (is-false (get-env-var vars "MISSING"))
    (is (equal "default" (get-env-var vars "MISSING" "default")))))

(test get-env-var-empty-alist
  "Test looking up from an empty alist"
  (is-false (get-env-var '() "KEY"))
  (is (equal "fallback" (get-env-var '() "KEY" "fallback"))))

;;; --- parse-env-var-integer ---

(test parse-env-var-integer-valid
  "Test parsing a valid integer env var"
  (let ((vars '(("PORT" . "8080"))))
    (is (= 8080 (parse-env-var-integer vars "PORT" 3000)))))

(test parse-env-var-integer-invalid
  "Test parsing an invalid integer env var returns default"
  (let ((vars '(("PORT" . "not-a-number"))))
    (is (= 3000 (parse-env-var-integer vars "PORT" 3000)))))

(test parse-env-var-integer-missing
  "Test parsing a missing integer env var uses default"
  (let ((vars '()))
    (is (= 3000 (parse-env-var-integer vars "PORT" 3000)))))

;;; --- read-env-file ---

(test read-env-file-existing
  "Test reading the actual config.env file"
  (let ((result (read-env-file #p"config.env")))
    (is (listp result))
    (is (assoc "PORT" result :test #'equal))
    (is (assoc "IP" result :test #'equal))))

(test read-env-file-nonexistent
  "Test reading a non-existent file returns nil"
  (is-false (read-env-file #p"/tmp/nonexistent-env-file-12345.env")))

;;; --- *config* structure ---

(test config-structure
  "Test that *config* has the expected structure"
  (is (listp *config*))
  (is (getf *config* :server))
  (is (numberp (getf (getf *config* :server) :port)))
  (is (stringp (getf (getf *config* :server) :address))))

(test env-vars-loaded
  "Test that *env-vars* is loaded from config.env"
  (is (listp *env-vars*))
  (is (assoc "PORT" *env-vars* :test #'equal)))

;;;; ============================================================
;;;; Server Module Tests (Pure Functions)
;;;; ============================================================

(def-suite server-tests
  :description "Unit tests for sbcl-web-service.core server"
  :in sbcl-web-service-tests)

(in-suite server-tests)

;;; --- extract-server-config ---

(test extract-server-config-valid
  "Test extracting server config from a plist"
  (let ((config '(:server (:port 8080 :address "127.0.0.1"))))
    (let ((server-config (extract-server-config config)))
      (is (= 8080 (getf server-config :port)))
      (is (equal "127.0.0.1" (getf server-config :address))))))

(test extract-server-config-missing
  "Test extracting server config when :server key is missing"
  (let ((config '(:other "data")))
    (is-false (extract-server-config config))))

;;; --- resolve-server-params ---

(test resolve-server-params-default
  "Test resolving server params from config without override"
  (let ((config '(:server (:port 8080 :address "127.0.0.1"))))
    (let ((params (resolve-server-params config)))
      (is (= 8080 (getf params :port)))
      (is (equal "127.0.0.1" (getf params :address)))
      (is (getf params :document-root)))))

(test resolve-server-params-with-override
  "Test resolving server params with port override"
  (let ((config '(:server (:port 8080 :address "127.0.0.1"))))
    (let ((params (resolve-server-params config 9090)))
      (is (= 9090 (getf params :port)))
      (is (equal "127.0.0.1" (getf params :address))))))

(test resolve-server-params-defaults-for-missing
  "Test that resolve-server-params provides defaults for missing values"
  (let ((config '(:server (:port 8080 :address "127.0.0.1"))))
    (let ((params (resolve-server-params config)))
      ;; access-log-destination and message-log-destination default to nil
      (is-false (getf params :access-log-destination))
      (is-false (getf params :message-log-destination)))))

;;; --- display-address ---

(test display-address-wildcard
  "Test that 0.0.0.0 maps to localhost"
  (is (equal "localhost" (display-address "0.0.0.0"))))

(test display-address-specific
  "Test that specific addresses are returned as-is"
  (is (equal "127.0.0.1" (display-address "127.0.0.1")))
  (is (equal "192.168.1.1" (display-address "192.168.1.1"))))

;;; --- make-acceptor ---

(test make-acceptor-creates-instance
  "Test that make-acceptor creates a Hunchentoot easy-acceptor"
  (let ((params '(:port 9999 :address "127.0.0.1"
                  :document-root #p"./static/"
                  :access-log-destination nil
                  :message-log-destination nil)))
    (let ((acceptor (make-acceptor params)))
      (is (typep acceptor 'hunchentoot:easy-acceptor)))))

;;; --- stop-server edge cases ---

(test stop-server-when-not-running
  "Test stopping server when no server is running returns t"
  (let ((*acceptor* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (is-true (stop-server)))))
      (declare (ignore output)))))

;;;; ============================================================
;;;; Web Route Tests
;;;; ============================================================

(def-suite web-route-tests
  :description "Unit tests for sbcl-web-service.web"
  :in sbcl-web-service-tests)

(in-suite web-route-tests)

;;; --- static-file-path ---

(test static-file-path-construction
  "Test that static-file-path constructs a valid pathname"
  (let ((path (static-file-path "index.html")))
    (is (pathnamep path))
    (is (search "static" (namestring path)))
    (is (search "index.html" (namestring path)))))

(test static-file-path-subdirectory
  "Test static-file-path with a subdirectory"
  (let ((path (static-file-path "css/style.css")))
    (is (pathnamep path))
    (is (search "static" (namestring path)))))

;;; --- route->dispatcher ---

(test route-to-dispatcher-prefix
  "Test that prefix routes create function dispatchers"
  (let ((route '(:prefix "/" . root-page)))
    (let ((dispatcher (route->dispatcher route)))
      (is (functionp dispatcher)))))

(test route-to-dispatcher-regex
  "Test that regex routes create function dispatchers"
  (let ((route '(:regex "^/test/.*" . handle-404)))
    (let ((dispatcher (route->dispatcher route)))
      (is (functionp dispatcher)))))

;;; --- web-routes->dispatchers ---

(test web-routes-to-dispatchers
  "Test that web-routes->dispatchers transforms all routes"
  (let ((dispatchers (web-routes->dispatchers *web-routes*)))
    (is (listp dispatchers))
    (is (= (length *web-routes*) (length dispatchers)))
    (is (every #'functionp dispatchers))))

;;; --- register-web-routes ---

(test register-web-routes-test
  "Test that register-web-routes adds dispatchers to dispatch table"
  (let ((hunchentoot:*dispatch-table* nil))
    (register-web-routes)
    (is (listp hunchentoot:*dispatch-table*))
    (is (= (length *web-routes*) (length hunchentoot:*dispatch-table*)))))

;;;; ============================================================
;;;; API Route Tests
;;;; ============================================================

(def-suite api-route-tests
  :description "Unit tests for sbcl-web-service.api"
  :in sbcl-web-service-tests)

(in-suite api-route-tests)

;;; --- api-routes->dispatchers ---

(test api-routes-to-dispatchers
  "Test that api-routes->dispatchers transforms all routes"
  (let ((dispatchers (api-routes->dispatchers *api-routes*)))
    (is (listp dispatchers))
    (is (= (length *api-routes*) (length dispatchers)))
    (is (every #'functionp dispatchers))))

;;; --- register-api-routes ---

(test register-api-routes-test
  "Test that register-api-routes adds dispatchers to dispatch table"
  (let ((hunchentoot:*dispatch-table* nil))
    (register-api-routes)
    (is (listp hunchentoot:*dispatch-table*))
    (is (= (length *api-routes*) (length hunchentoot:*dispatch-table*)))))

;;; --- api route data ---

(test api-routes-data-structure
  "Test that *api-routes* has expected entries"
  (is (listp *api-routes*))
  (is (assoc "/api/example" *api-routes* :test #'equal))
  (is (assoc "/api/health" *api-routes* :test #'equal)))

;;;; ============================================================
;;;; Main Module Tests
;;;; ============================================================

(def-suite main-tests
  :description "Unit tests for sbcl-web-service main module"
  :in sbcl-web-service-tests)

(in-suite main-tests)

(test build-dispatch-table-returns-list
  "Test that build-dispatch-table returns a non-empty list of dispatchers"
  (let ((table (build-dispatch-table)))
    (is (listp table))
    (is (> (length table) 0))
    (is (every #'functionp table))))

(test build-dispatch-table-has-all-routes
  "Test that build-dispatch-table includes both web and API routes"
  (let ((table (build-dispatch-table)))
    (is (= (+ (length *web-routes*) (length *api-routes*))
            (length table)))))

(test initialize-routes-sets-dispatch-table
  "Test that initialize-routes sets the Hunchentoot dispatch table"
  (let ((old-table hunchentoot:*dispatch-table*))
    (is-true (initialize-routes))
    (is (listp hunchentoot:*dispatch-table*))
    (is (> (length hunchentoot:*dispatch-table*) 0))
    ;; Restore
    (setf hunchentoot:*dispatch-table* old-table)))

;;;; ============================================================
;;;; Integration Tests (HTTP)
;;;; ============================================================

(def-suite integration-tests
  :description "HTTP integration tests requiring a running server"
  :in sbcl-web-service-tests)

(in-suite integration-tests)

(test server-startup
  "Test that the server starts up correctly and *acceptor* is set"
  (call-with-test-server
   (lambda (port)
     (declare (ignore port))
     (is-true *acceptor*)
     (is (typep *acceptor* 'hunchentoot:easy-acceptor)))))

(test server-restart
  "Test that starting a server when one is running restarts cleanly"
  (call-with-test-server
   (lambda (port)
     (declare (ignore port))
     ;; Start another server on a different port - this should stop the first
     (let ((output (with-output-to-string (*standard-output*)
                     (start-server 8089))))
       (declare (ignore output))
       (is-true *acceptor*)
       (stop-server)))))

(test hello-world-endpoint
  "Test that the root endpoint returns HTML with Hello, World!"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/") :force-binary nil)
       (is (= status 200))
       (is (search "Hello, World!" body))))))

(test api-endpoint
  "Test that the API endpoint returns valid JSON with expected fields"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/api/example"))
       (is (= status 200))
       (let ((json-response (json:decode-json-from-string (ensure-string body))))
         (is (equal "success" (cdr (assoc :status json-response))))
         (is (equal "API is working" (cdr (assoc :message json-response))))
         (is (assoc :data json-response)))))))

(test health-endpoint
  "Test that the health endpoint returns valid JSON with status, version, timestamp"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/api/health"))
       (is (= status 200))
       (let ((json-response (json:decode-json-from-string (ensure-string body))))
         (is (equal "ok" (cdr (assoc :status json-response))))
         (is (assoc :version json-response))
         (is (assoc :timestamp json-response)))))))

(test health-endpoint-timestamp-format
  "Test that the health endpoint timestamp is in ISO 8601 format"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/api/health"))
       (declare (ignore status))
       (let* ((json-response (json:decode-json-from-string (ensure-string body)))
              (timestamp (cdr (assoc :timestamp json-response))))
         (is (stringp timestamp))
         (is (= 20 (length timestamp)))
         (is (char= #\Z (char timestamp 19))))))))

(test not-found-handling
  "Test that unknown routes return 404 with error page"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status)
         (http-request (test-url port "/unknown-route") :force-binary nil)
       (is (= status 404))
       (is (search "Page Not Found" body))))))

(test api-endpoint-content-type
  "Test that API endpoints return application/json content type"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status headers)
         (http-request (test-url port "/api/example"))
       (declare (ignore body))
       (is (= status 200))
       (let ((content-type (cdr (assoc :content-type headers))))
         (is (search "application/json" content-type)))))))

(test root-endpoint-content-type
  "Test that the root endpoint returns text/html content type"
  (call-with-test-server
   (lambda (port)
     (multiple-value-bind (body status headers)
         (http-request (test-url port "/") :force-binary nil)
       (declare (ignore body))
       (is (= status 200))
       (let ((content-type (cdr (assoc :content-type headers))))
         (is (search "text/html" content-type)))))))

;;;; ============================================================
;;;; Test Runners
;;;; ============================================================

(defun run-tests ()
  (format t "Running tests for the SBCL web service...~%")
  (run! 'sbcl-web-service-tests)
  (format t "Tests completed.~%"))

(defun run-tests-interactive ()
  (format t "Running tests for the SBCL web service interactively...~%")
  (run! 'sbcl-web-service-tests)
  (explain! (get-test 'sbcl-web-service-tests))
  (format t "Interactive tests completed.~%"))
