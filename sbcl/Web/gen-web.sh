#!/bin/bash

# SBCL Web Service Project Generator (using Hunchentoot)
# Usage: ./gen-web.sh <project-name>

set -e

# Check if project name is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <project-name>"
    echo "Example: $0 my-web-service"
    exit 1
fi

PROJECT_NAME="$1"

# Check if directory already exists
if [ -d "$PROJECT_NAME" ]; then
    echo "Error: Directory '$PROJECT_NAME' already exists"
    exit 1
fi

echo "Creating SBCL Web Service project: $PROJECT_NAME"
echo "=========================================="

# Create project directory structure
mkdir -p "$PROJECT_NAME/src/utils"
mkdir -p "$PROJECT_NAME/src/core"
mkdir -p "$PROJECT_NAME/src/web"
mkdir -p "$PROJECT_NAME/src/api"
mkdir -p "$PROJECT_NAME/tests"
mkdir -p "$PROJECT_NAME/static"

# Create .gitignore
cat > "$PROJECT_NAME/.gitignore" << 'EOF'
*.FASL
*.fasl
*.lisp-temp
*.dfsl
*.pfsl
*.d64fsl
*.p64fsl
*.lx64fsl
*.lx32fsl
*.dx64fsl
*.dx32fsl
*.fx64fsl
*.fx32fsl
*.sx64fsl
*.sx32fsl
*.wx64fsl
*.wx32fsl
*.abcl
*.x86f
*~
.#*
# Ignore the quicklisp directory
**/quicklisp/
# Coverage
**/coverage-report/
EOF

# Create .tool-versions
cat > "$PROJECT_NAME/.tool-versions" << 'EOF'
sbcl 2.6.0

EOF

# Copy quicklisp.lisp from current directory
if [ -f "quicklisp.lisp" ]; then
    cp "quicklisp.lisp" "$PROJECT_NAME/quicklisp.lisp"
    echo "✓ Copied quicklisp.lisp"
else
    echo "Warning: quicklisp.lisp not found in current directory"
    echo "You may need to download it manually from https://beta.quicklisp.org/quicklisp.lisp"
fi

# Create config.env
cat > "$PROJECT_NAME/config.env" << 'EOF'
PORT=8080
IP=0.0.0.0
EOF

# Create README.md
cat > "$PROJECT_NAME/README.md" << EOF
# $PROJECT_NAME

이 프로젝트는 Steel Bank Common Lisp (SBCL)를 사용하여 구축된 웹 서비스입니다. 모듈성과 사용 편의성에 중점을 둔 웹 애플리케이션 생성 및 관리를 위한 프레임워크를 제공합니다.

## 프로젝트 구조

\`\`\`
$PROJECT_NAME/
├── .gitignore
├── .tool-versions
├── README.md
├── CLAUDE.md
├── config.env
├── quicklisp.lisp
├── setup-quicklisp.lisp
├── initialize.lisp
├── run-server.sh
├── run-tests.sh
├── run-coverage.sh
├── $PROJECT_NAME.asd
├── static/
│   ├── index.html
│   └── error.html
├── src/
│   ├── utils/
│   │   └── utils.lisp
│   ├── core/
│   │   ├── config.lisp
│   │   └── server.lisp
│   ├── web/
│   │   └── routes.lisp
│   ├── api/
│   │   └── routes.lisp
│   └── main.lisp
└── tests/
    └── test-suite.lisp
\`\`\`

## Prerequisites

- SBCL (Steel Bank Common Lisp)

## 1. Setup Quicklisp

\`\`\`bash
cd $PROJECT_NAME
sbcl --load setup-quicklisp.lisp --quit
\`\`\`

## 2. Start the server

\`\`\`bash
./run-server.sh
# 또는
sbcl --load initialize.lisp
\`\`\`

그런 다음 \`http://localhost:8080\`에서 웹 서비스에 접속할 수 있습니다.

## 3. Run tests

\`\`\`bash
./run-tests.sh
\`\`\`

## 4. Run coverage

\`\`\`bash
./run-coverage.sh
\`\`\`

## Available Endpoints

| Endpoint | Description |
|----------|-------------|
| / | Hello World 페이지 |
| /api/example | JSON API 예제 |
| /api/health | 헬스 체크 |

## References

- [Hunchentoot](https://edicl.github.io/hunchentoot/) - Common Lisp 웹 서버
- [FiveAM](https://github.com/lispci/fiveam) - 테스트 프레임워크
- [Quicklisp](https://www.quicklisp.org/)
- [SBCL](http://www.sbcl.org/)
EOF

# Create CLAUDE.md
cat > "$PROJECT_NAME/CLAUDE.md" << EOF
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands

**Prerequisites:** SBCL and Quicklisp must be installed. First-time setup:
\`\`\`bash
sbcl --load setup-quicklisp.lisp --quit
\`\`\`

**Start the server:**
\`\`\`bash
./run-server.sh
# or directly:
sbcl --load initialize.lisp
\`\`\`

**Run tests:**
\`\`\`bash
./run-tests.sh
\`\`\`

**Server configuration:** \`config.env\` (KEY=VALUE format, PORT and IP).

## Architecture

This is an SBCL/Hunchentoot web service using ASDF for system definition and Quicklisp for dependency management.

### Package/Module Hierarchy

\`\`\`
$PROJECT_NAME          (main entry point, composes all modules)
├── $PROJECT_NAME.core  (server lifecycle + config loading)
├── $PROJECT_NAME.utils (logging, JSON helpers, timestamps)
├── $PROJECT_NAME.api   (JSON API route handlers)
└── $PROJECT_NAME.web   (HTML page route handlers)
\`\`\`

### Request Flow

1. \`initialize.lisp\` bootstraps Quicklisp → loads ASDF system → calls \`main\`
2. \`main\` (src/main.lisp) loads dependencies → calls \`initialize-routes\` → calls \`start-server\`
3. \`initialize-routes\` sets \`*dispatch-table*\` with API routes first, then web routes (order matters for prefix matching)
4. Hunchentoot dispatches requests via \`*dispatch-table*\` to handlers defined in \`src/api/routes.lisp\` and \`src/web/routes.lisp\`

### Key Patterns

- **Global state:** \`*acceptor*\` holds the running server instance; \`*config*\` holds plist-based configuration read from \`config.env\` at load time
- **Route registration:** Handlers are defined with \`hunchentoot:define-easy-handler\`, then registered into \`*dispatch-table*\` via prefix/regex dispatchers
- **Logging:** Custom level-based logging in utils (\`:debug :info :warn :error :fatal\`) outputs to stdout with ISO 8601 timestamps
- **JSON:** Uses \`cl-json\` for encoding/decoding; \`json-response\` sets content-type and status code
- **Tests:** FiveAM test suite in \`tests/test-suite.lisp\`; tests start/stop a real server on port 8088 and make HTTP requests via Drakma

### Dependencies

Defined in \`$PROJECT_NAME.asd\`: hunchentoot, cl-json, alexandria, cl-ppcre, cl-utilities, split-sequence. Test-only: fiveam, drakma.

## Conventions

- All source code is in \`src/\` organized by module
- Static assets (HTML) are served from \`static/\`
- The \`.asd\` file is the source of truth for module load order (\`:serial t\`)
EOF

# Create setup-quicklisp.lisp
cat > "$PROJECT_NAME/setup-quicklisp.lisp" << 'EOF'
;;;; setup-quicklisp.lisp
;;;; Script to install and set up Quicklisp

(format t "~%Setting up Quicklisp for Common Lisp Web Application...~%")

;; Check if Quicklisp is already installed in project directory
(if (probe-file "quicklisp/setup.lisp")
    (format t "Quicklisp already installed in project directory.~%")
    (progn
      (format t "Downloading and installing Quicklisp...~%")

      ;; Download Quicklisp if not present
      (unless (probe-file "quicklisp.lisp")
        (format t "Downloading quicklisp.lisp...~%")
        (let ((quicklisp-url "https://beta.quicklisp.org/quicklisp.lisp"))
          #+sbcl
          (sb-ext:run-program "curl"
                              (list "-o" "quicklisp.lisp" "-L" quicklisp-url)
                              :search t
                              :output t
                              :error t)
          (unless (probe-file "quicklisp.lisp")
            (format t "Error: Failed to download quicklisp.lisp~%")
            (sb-ext:exit :code 1))))

      ;; Load Quicklisp
      (load "quicklisp.lisp")

      ;; Install Quicklisp into the project directory
      (handler-case
          (progn
            (funcall (intern "INSTALL" (find-package "QUICKLISP-QUICKSTART"))
                     :path (merge-pathnames "quicklisp/" (truename ".")))
            (format t "Quicklisp installed successfully.~%"))
        (error (e)
          (format t "Error installing Quicklisp: ~A~%" e)
          (sb-ext:exit :code 1)))))

;; Load Quicklisp from project directory
(format t "Loading Quicklisp from project directory...~%")
(handler-case
    (load (merge-pathnames "quicklisp/setup.lisp" (truename ".")))
  (error (e)
    (format t "Error loading Quicklisp: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Install required packages
(format t "Installing required packages...~%")
(handler-case
    (progn
      (funcall (intern "QUICKLOAD" (find-package "QL"))
               '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities :split-sequence :fiveam :drakma))
      (format t "All packages installed successfully.~%"))
  (error (e)
    (format t "Error installing packages: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Quicklisp setup complete.~%")

EOF

# Create project .asd file
cat > "$PROJECT_NAME/$PROJECT_NAME.asd" << EOF
(defsystem "$PROJECT_NAME"
  :description "A web service built with Steel Bank Common Lisp."
  :author ""
  :license "BSD 3-Clause"
  :depends-on ("hunchentoot" "cl-json" "alexandria" "cl-ppcre" "cl-utilities" "split-sequence")
  :components ((:module "src"
                :serial t
                :components
                ((:module "utils"
                  :components ((:file "utils")))
                 (:module "core"
                  :serial t
                  :components ((:file "config")
                             (:file "server")))
                 (:module "web"
                  :components ((:file "routes")))
                 (:module "api"
                  :components ((:file "routes")))
                 (:file "main")))))

(defsystem "$PROJECT_NAME/tests"
  :description "Test system for $PROJECT_NAME"
  :author ""
  :license "BSD 3-Clause"
  :depends-on ("$PROJECT_NAME" "fiveam" "drakma")
  :components ((:module "tests"
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (symbol-call :$PROJECT_NAME.tests :run-tests)))

EOF

# ============================================================
# Source files use __PROJECT_NAME__ placeholder + sed replacement
# to avoid shell expansion issues with Lisp special characters
# ============================================================

# Create src/utils/utils.lisp
cat > "$PROJECT_NAME/src/utils/utils.lisp" << 'UTILSEOF'
(defpackage :__PROJECT_NAME__.utils
  (:documentation "Utility functions for the web service.")
  (:use :cl :split-sequence)
  (:export ;; Logging
           :*log-levels*
           :*current-log-level*
           :log-level-enabled-p
           :log-message
           :log-debug
           :log-info
           :log-warn
           :log-error
           :log-fatal
           ;; Time utilities
           :get-timestamp
           ;; JSON utilities
           :encode-json-body
           :parse-json-safely
           :json-response))

(in-package :__PROJECT_NAME__.utils)

;;;; Date and Time Utilities

(defun get-timestamp ()
  "Return the current timestamp in ISO 8601 format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month date hour minute second)))

;;;; Logging System

(defparameter *log-levels* '(:debug :info :warn :error :fatal)
  "Available logging levels in order of severity.")

(defparameter *current-log-level* :info
  "Current log level threshold. Messages below this level won't be logged.")

(defun log-level-enabled-p (level)
  "Check if the specified log level is enabled based on current threshold."
  (let ((current-pos (position *current-log-level* *log-levels*))
        (level-pos (position level *log-levels*)))
    (and current-pos level-pos (>= level-pos current-pos))))

(defun log-message (level message &rest args)
  "Log a message with the specified level if that level is enabled."
  (when (log-level-enabled-p level)
    (let ((timestamp (get-timestamp))
          (formatted-message (apply #'format nil message args)))
      (format t "[~A] [~A] ~A~%" timestamp (string-upcase (symbol-name level)) formatted-message))))

;; Convenience logging functions
(defun log-debug (message &rest args)
  "Log a debug message."
  (apply #'log-message :debug message args))

(defun log-info (message &rest args)
  "Log an info message."
  (apply #'log-message :info message args))

(defun log-warn (message &rest args)
  "Log a warning message."
  (apply #'log-message :warn message args))

(defun log-error (message &rest args)
  "Log an error message."
  (apply #'log-message :error message args))

(defun log-fatal (message &rest args)
  "Log a fatal message."
  (apply #'log-message :fatal message args))

;;;; JSON Utilities

(defun parse-json-safely (json-string)
  "Safely parse a JSON string, returning NIL on error."
  (handler-case (json:decode-json-from-string json-string)
    (error (e)
      (log-error "Failed to parse JSON: ~A" e)
      nil)))

(defun encode-json-body (data)
  "Encode data as a JSON string. Pure function with no side effects."
  (json:encode-json-to-string data))

(defun json-response (data &optional (status 200))
  "Set HTTP response headers and return JSON-encoded body.
   Side effects: sets content-type and return-code via Hunchentoot."
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (encode-json-body data))
UTILSEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/utils/utils.lisp"

# Create src/core/config.lisp
cat > "$PROJECT_NAME/src/core/config.lisp" << 'CONFIGEOF'
(defpackage :__PROJECT_NAME__.core
  (:documentation "Core functionality for the web service.")
  (:use :cl :split-sequence)
  (:export ;; Configuration (pure)
           :*config*
           :*env-vars*
           :parse-env-line
           :get-env-var
           :read-env-file
           :parse-env-var-integer
           ;; Server - pure functions
           :extract-server-config
           :resolve-server-params
           :make-acceptor
           :display-address
           ;; Server - effect functions
           :start-acceptor
           :stop-acceptor
           ;; Server - stateful shell
           :start-server
           :stop-server
           :*acceptor*))

(in-package :__PROJECT_NAME__.core)

;;;; Environment Configuration

(defun parse-env-line (line)
  "Parse a single KEY=VALUE line into a cons cell, or NIL if invalid.
   Pure function: no side effects."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (pos (position #\= trimmed)))
    (when (and (plusp (length trimmed))
               (not (char= (char trimmed 0) #\#))
               pos (> pos 0))
      (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
            (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos)))))))

(defun read-env-file (file-path)
  "Read environment variables from a file in KEY=VALUE format.
   Returns an immutable alist of (key . value) string pairs."
  (handler-case
      (with-open-file (stream file-path :if-does-not-exist nil)
        (when stream
          (loop for line = (read-line stream nil nil)
                while line
                for pair = (parse-env-line line)
                when pair collect pair)))
    (error (e)
      (format *error-output* "Error reading environment file ~A: ~A~%" file-path e)
      nil)))

(defun get-env-var (env-vars key &optional default)
  "Look up an environment variable from an alist.
   Pure function: returns the value or default without side effects."
  (let ((pair (assoc key env-vars :test #'equal)))
    (if pair (cdr pair) default)))

(defun parse-env-var-integer (env-vars key &optional default)
  "Parse an environment variable as an integer with error handling."
  (handler-case
      (parse-integer (get-env-var env-vars key (princ-to-string default)))
    (error (e)
      (format *error-output* "Error parsing ~A as integer: ~A. Using default ~A.~%" key e default)
      default)))

;;;; Application Configuration

;; Read environment variables from config.env file
(defparameter *env-vars* (read-env-file #p"config.env"))

;; Server configuration parameters
(defparameter *config*
  `(:server (:port ,(parse-env-var-integer *env-vars* "PORT" 8080)
             :address ,(get-env-var *env-vars* "IP" "127.0.0.1")
             :document-root #p"./static/"
             :access-log-destination nil
             :message-log-destination nil)))
CONFIGEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/core/config.lisp"

# Create src/core/server.lisp
cat > "$PROJECT_NAME/src/core/server.lisp" << 'SERVEREOF'
(in-package :__PROJECT_NAME__.core)

;;;; Server Management
;;;; Organized as: Pure Functions → Effect Functions → Stateful Shell

;;; --- Pure Functions (no side effects, no global state access) ---

(defun extract-server-config (config)
  "Extract the server sub-configuration from a config plist.
   Pure function: takes config explicitly, does not read globals."
  (getf config :server))

(defun resolve-server-params (config &optional override-port)
  "Resolve server parameters from config, with optional port override.
   Returns a plist (:port port :address address :document-root root ...).
   Pure function."
  (let ((server-config (extract-server-config config)))
    (list :port (or override-port (getf server-config :port))
          :address (getf server-config :address)
          :document-root (getf server-config :document-root #p"./static/")
          :access-log-destination (getf server-config :access-log-destination nil)
          :message-log-destination (getf server-config :message-log-destination nil))))

(defun make-acceptor (params)
  "Create a new Hunchentoot acceptor from resolved server params.
   Pure function: allocates an object but does not start it."
  (make-instance 'hunchentoot:easy-acceptor
                 :port (getf params :port)
                 :address (getf params :address)
                 :document-root (getf params :document-root)
                 :access-log-destination (getf params :access-log-destination)
                 :message-log-destination (getf params :message-log-destination)))

(defun display-address (address)
  "Convert bind address to a human-friendly display address.
   Pure function."
  (if (string= address "0.0.0.0") "localhost" address))

;;; --- Effect Functions (minimal side effects, no global state mutation) ---

(defun start-acceptor (acceptor)
  "Start a Hunchentoot acceptor. Side effect: begins listening on the network.
   Returns the started acceptor."
  (hunchentoot:start acceptor)
  acceptor)

(defun stop-acceptor (acceptor)
  "Stop a Hunchentoot acceptor. Side effect: stops listening on the network.
   Returns T on success, NIL on failure."
  (handler-case
      (progn
        (hunchentoot:stop acceptor)
        t)
    (error (e)
      (__PROJECT_NAME__.utils:log-error "Error stopping server: ~A" e)
      nil)))

;;; --- Stateful Shell (manages *acceptor* global, composes pure + effect) ---

(defvar *acceptor* nil
  "Global variable to hold the running Hunchentoot acceptor instance.")

(defun start-server (&optional port)
  "Start the web server. Composes pure config resolution, acceptor creation,
   and effectful server start. Manages global *acceptor* state."
  (let* ((params (resolve-server-params *config* port))
         (server-port (getf params :port))
         (server-address (getf params :address)))

    (__PROJECT_NAME__.utils:log-info "Starting the web service on port ~A..." server-port)

    (when *acceptor*
      (__PROJECT_NAME__.utils:log-info "Server is already running. Stopping it first...")
      (stop-server))

    (handler-case
        (let ((acceptor (start-acceptor (make-acceptor params))))
          (setf *acceptor* acceptor)
          (__PROJECT_NAME__.utils:log-info "Server started successfully on port ~A!" server-port)
          (__PROJECT_NAME__.utils:log-info "Visit http://~A:~A/ to see the application"
                   (display-address server-address) server-port)
          acceptor)
      (error (e)
        (__PROJECT_NAME__.utils:log-error "Failed to start server: ~A" e)
        (setf *acceptor* nil)
        nil))))

(defun stop-server ()
  "Stop the web server if running. Manages global *acceptor* state."
  (if *acceptor*
      (progn
        (__PROJECT_NAME__.utils:log-info "Stopping the server...")
        (let ((result (stop-acceptor *acceptor*)))
          (setf *acceptor* nil)
          (if result
              (progn (__PROJECT_NAME__.utils:log-info "Server stopped.") t)
              nil)))
      (progn
        (__PROJECT_NAME__.utils:log-warn "No server running, nothing to stop.")
        t)))
SERVEREOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/core/server.lisp"

# Create src/web/routes.lisp
cat > "$PROJECT_NAME/src/web/routes.lisp" << 'WEBROUTESEOF'
(defpackage :__PROJECT_NAME__.web
  (:documentation "Web routes and handlers for the web service.")
  (:use :cl :__PROJECT_NAME__.utils)
  (:export :*web-routes*
           :web-routes->dispatchers
           :route->dispatcher
           :register-web-routes
           :root-page
           :handle-404
           :static-file-path))

(in-package :__PROJECT_NAME__.web)

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
WEBROUTESEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/web/routes.lisp"

# Create src/api/routes.lisp
cat > "$PROJECT_NAME/src/api/routes.lisp" << 'APIROUTESEOF'
(defpackage :__PROJECT_NAME__.api
  (:documentation "API endpoints for the web service.")
  (:use :cl :__PROJECT_NAME__.utils)
  (:export :*api-routes*
           :api-routes->dispatchers
           :register-api-routes
           :api-example
           :health-check))

(in-package :__PROJECT_NAME__.api)

;;;; API Route Definitions

;; API example endpoint
(hunchentoot:define-easy-handler (api-example :uri "/api/example") ()
  "Return a sample JSON response for API testing."
  (__PROJECT_NAME__.utils:json-response
   '((:status . "success")
     (:message . "API is working")
     (:data . ((:id . 1) (:name . "Example"))))))

;; Health check endpoint
(hunchentoot:define-easy-handler (health-check :uri "/api/health") ()
  "Return server health status information."
  (__PROJECT_NAME__.utils:json-response
   `((:status . "ok")
     (:version . "1.0.0")
     (:timestamp . ,(__PROJECT_NAME__.utils:get-timestamp)))))

;;;; API Route Table (declarative data)

(defparameter *api-routes*
  '(("/api/example" . api-example)
    ("/api/health"  . health-check))
  "Declarative API route definitions as an alist of (prefix . handler).")

;;;; API Route Registration

(defun api-routes->dispatchers (routes)
  "Transform an alist of route definitions into Hunchentoot dispatchers.
   Pure function: returns a list without modifying global state."
  (mapcar (lambda (route)
            (hunchentoot:create-prefix-dispatcher (car route) (cdr route)))
          routes))

(defun register-api-routes ()
  "Register all API routes in the Hunchentoot dispatch table."
  (setf hunchentoot:*dispatch-table*
        (append (api-routes->dispatchers *api-routes*)
                hunchentoot:*dispatch-table*)))
APIROUTESEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/api/routes.lisp"

# Create src/main.lisp
cat > "$PROJECT_NAME/src/main.lisp" << 'MAINEOF'
(defpackage :__PROJECT_NAME__
  (:documentation "A web service built with Steel Bank Common Lisp.")
  (:use :cl :split-sequence
        :__PROJECT_NAME__.core
        :__PROJECT_NAME__.utils
        :__PROJECT_NAME__.api
        :__PROJECT_NAME__.web)
  (:export ;; Main entry point
           :main
           :load-dependencies
           :build-dispatch-table
           :initialize-routes
           ;; Re-export core functionality
           :start-server
           :stop-server
           :*config*
           :*acceptor*
           :get-env-var
           :read-env-file))

(in-package :__PROJECT_NAME__)

;;;; Main Application Entry Point

(defun load-dependencies (&optional (deps '(:hunchentoot :cl-json :alexandria
                                            :cl-ppcre :cl-utilities :split-sequence)))
  "Load all required dependencies for the application.
   Accepts an optional list of dependency names for testability."
  (handler-case
      (progn
        (ql:quickload deps :silent t)
        t)  ; Return T on success
    (error (e)
      (format *error-output* "Error loading dependencies: ~A~%" e)
      nil)))  ; Return NIL on failure

(defun build-dispatch-table ()
  "Build a complete dispatch table by composing all route modules.
   API routes come first for specific path matching, then web routes.
   Pure function: returns a new dispatch table list without side effects."
  (append (__PROJECT_NAME__.api:api-routes->dispatchers
           __PROJECT_NAME__.api:*api-routes*)
          (__PROJECT_NAME__.web:web-routes->dispatchers
           __PROJECT_NAME__.web:*web-routes*)))

(defun initialize-routes ()
  "Install the composed dispatch table into Hunchentoot.
   Single point of side effect for route registration."
  (setf hunchentoot:*dispatch-table* (build-dispatch-table))
  t)

;; Main function to start the application
(defun main ()
  "Start the web service application.
   This is the main entry point for the application."
  (log-info "Starting Web Service Application")

  ;; Ensure dependencies are loaded
  (unless (load-dependencies)
    (log-error "Failed to load dependencies. Aborting startup.")
    (return-from main nil))

  ;; Initialize routes
  (initialize-routes)

  ;; Start the server
  (let ((server (start-server)))
    (if server
        (log-info "Application started successfully!")
        (log-error "Failed to start the application."))
    server))
MAINEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/main.lisp"

# Create initialize.lisp
cat > "$PROJECT_NAME/initialize.lisp" << 'INITEOF'
;;;; Web Service Initialize Script

;; This script loads and starts the web service application
;; It handles dependency loading, system initialization, and provides helpful information

;;;; System Setup

;; Make sure ASDF is loaded
(require :asdf)

;; Add the current directory to ASDF's search path
(push (truename ".") asdf:*central-registry*)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (truename "."))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Define a flag to track overall success
(defvar *initialize-success* t)

;;;; Dependency Management

(format t "~%Installing required dependencies...~%")
(handler-case
    (progn
      (ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities :split-sequence :fiveam :drakma) :silent t)
      (format t "Dependencies installed successfully.~%"))
  (error (e)
    (format *error-output* "Error installing dependencies: ~A~%" e)
    (format t "Failed to install dependencies. Please check your Quicklisp installation.~%")
    (setf *initialize-success* nil)))

;;;; System Loading

;; Only proceed if dependencies loaded successfully
(when *initialize-success*
  (format t "Loading the web service system...~%")
  (handler-case
      (asdf:load-system :__PROJECT_NAME__)
    (error (e)
      (format *error-output* "Error loading system: ~A~%" e)
      (format t "Failed to load the system. Please check for errors in the codebase.~%")
      (setf *initialize-success* nil))))

;;;; Application Initialize

;; Only proceed if system loaded successfully
(when *initialize-success*
  (format t "~%Starting the web service...~%")
  (let ((server (__PROJECT_NAME__:main)))
    (unless server
      (format t "Failed to start the server. Check the logs for errors.~%")
      (setf *initialize-success* nil))))

;;;; Server Information

;; Only display server information if everything started successfully
(when *initialize-success*
  (let* ((env-vars (__PROJECT_NAME__:read-env-file #p"config.env"))
         (port (__PROJECT_NAME__:get-env-var env-vars "PORT" "8080"))
         (host (__PROJECT_NAME__:get-env-var env-vars "IP" "127.0.0.1")))

    ;; Print a helpful message with server information
    (format t "~%Web service is running. Access it at http://~A:~A/~%"
            (if (string= host "0.0.0.0") "localhost" host)
            port)
    (format t "Available endpoints:~%")
    (format t "  / - Hello World~%")
    (format t "  /api/example - JSON API example~%")
    (format t "  /api/health - Health check endpoint~%")
    (format t "~%To stop the server, call (__PROJECT_NAME__:stop-server)~%")))
INITEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/initialize.lisp"

# Create run-server.sh
# This needs special handling: the inner run-server.lisp heredoc contains the project name
cat > "$PROJECT_NAME/run-server.sh" << 'RUNSERVEREOF'
#!/bin/bash

# Check if Quicklisp is installed, if not, install it
if [ ! -f "quicklisp/setup.lisp" ]; then
    echo "Quicklisp not found. Installing..."
    sbcl --load setup-quicklisp.lisp --quit
    if [ $? -ne 0 ]; then
        echo "Failed to install Quicklisp. Exiting."
        exit 1
    fi
    echo "Quicklisp installed successfully."
fi

# Run SBCL with the initialize script
sbcl --load initialize.lisp
RUNSERVEREOF

chmod +x "$PROJECT_NAME/run-server.sh"

# Create run-tests.sh
cat > "$PROJECT_NAME/run-tests.sh" << EOF
#!/bin/bash
cd "\$(dirname "\$0")"
sbcl --eval "(require :asdf)" \\
     --eval "(push (truename \".\") asdf:*central-registry*)" \\
     --eval "(load (merge-pathnames \"quicklisp/setup.lisp\" (truename \".\")))" \\
     --eval "(ql:quickload '(:fiveam :drakma) :silent t)" \\
     --eval "(ql:quickload :$PROJECT_NAME/tests :silent t)" \\
     --eval "($PROJECT_NAME.tests:run-tests)" \\
     --quit
EOF

chmod +x "$PROJECT_NAME/run-tests.sh"

# Create run-coverage.sh
cat > "$PROJECT_NAME/run-coverage.sh" << 'COVERAGEEOF'
#!/bin/bash
# SBCL 코드 커버리지 측정 스크립트
# sb-cover를 사용하여 테스트 커버리지 리포트를 HTML로 생성합니다.

cd "$(dirname "$0")"

REPORT_DIR="coverage-report"

# 이전 리포트 정리
rm -rf "$REPORT_DIR"
mkdir -p "$REPORT_DIR"

echo "=== SBCL 코드 커버리지 측정 시작 ==="
echo ""

sbcl --noinform \
     --eval "(require :asdf)" \
     --eval "(require :sb-cover)" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(load (merge-pathnames \"quicklisp/setup.lisp\" (truename \".\")))" \
     --eval "(declaim (optimize sb-cover:store-coverage-data))" \
     --eval "(ql:quickload '(:fiveam :drakma) :silent t)" \
     --eval "(asdf:load-system :__PROJECT_NAME__ :force t)" \
     --eval "(asdf:load-system :__PROJECT_NAME__/tests :force t)" \
     --eval "(__PROJECT_NAME__.tests:run-tests)" \
     --eval "(sb-cover:report \"$REPORT_DIR/\")" \
     --eval "(format t \"~%=== 커버리지 리포트 생성 완료 ===~%\")" \
     --eval "(format t \"리포트 위치: $(pwd)/$REPORT_DIR/cover-index.html~%\")" \
     --eval "(declaim (optimize (sb-cover:store-coverage-data 0)))" \
     --eval "(sb-cover:reset-coverage)" \
     --quit

echo ""
echo "브라우저에서 열기: file://$(pwd)/$REPORT_DIR/cover-index.html"
COVERAGEEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/run-coverage.sh"

chmod +x "$PROJECT_NAME/run-coverage.sh"

# Create static/index.html
cat > "$PROJECT_NAME/static/index.html" << EOF
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>$PROJECT_NAME</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
            background-color: #f5f5f5;
        }
        .container {
            text-align: center;
            padding: 2rem;
            background-color: white;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        h1 {
            color: #333;
        }
        p {
            color: #666;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Hello, World!</h1>
        <p>Welcome to $PROJECT_NAME</p>
    </div>
</body>
</html>
EOF

# Create static/error.html
cat > "$PROJECT_NAME/static/error.html" << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Page Not Found</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            height: 100vh;
            background-color: #f8f8f8;
        }
        .container {
            text-align: center;
            padding: 2rem;
            background-color: white;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            max-width: 500px;
            width: 90%;
        }
        h1 {
            color: #e74c3c;
            margin-bottom: 1rem;
        }
        p {
            color: #666;
            margin-bottom: 1.5rem;
        }
        .back-link {
            display: inline-block;
            background-color: #3498db;
            color: white;
            padding: 10px 20px;
            text-decoration: none;
            border-radius: 4px;
            transition: background-color 0.3s;
        }
        .back-link:hover {
            background-color: #2980b9;
        }
        .error-code {
            font-size: 5rem;
            font-weight: bold;
            color: #e74c3c;
            margin: 0;
            line-height: 1;
            opacity: 0.5;
        }
    </style>
</head>
<body>
    <div class="container">
        <p class="error-code">404</p>
        <h1>Page Not Found</h1>
        <p>Request URL path does not exist. Please check the URL and try again.</p>
        <a href="/" class="back-link">Go back to Home</a>
    </div>
</body>
</html>
EOF

# Create tests/test-suite.lisp
cat > "$PROJECT_NAME/tests/test-suite.lisp" << 'TESTEOF'
(defpackage :__PROJECT_NAME__.tests
  (:use :cl :fiveam)
  (:import-from :drakma :http-request)
  ;; Main package
  (:import-from :__PROJECT_NAME__
                :start-server :stop-server :*config* :*acceptor*
                :load-dependencies
                :build-dispatch-table :initialize-routes :main)
  ;; Utils
  (:import-from :__PROJECT_NAME__.utils
                :get-timestamp
                :*log-levels* :*current-log-level*
                :log-level-enabled-p :log-message
                :log-debug :log-info :log-warn :log-error :log-fatal
                :parse-json-safely :encode-json-body :json-response)
  ;; Core - config
  (:import-from :__PROJECT_NAME__.core
                :parse-env-line :read-env-file :get-env-var
                :parse-env-var-integer :*env-vars*
                :extract-server-config :resolve-server-params
                :make-acceptor :display-address
                :start-acceptor :stop-acceptor)
  ;; Web routes
  (:import-from :__PROJECT_NAME__.web
                :static-file-path :route->dispatcher :web-routes->dispatchers
                :*web-routes* :register-web-routes)
  ;; API routes
  (:import-from :__PROJECT_NAME__.api
                :api-routes->dispatchers :*api-routes* :register-api-routes)
  (:export :run-tests))

(in-package :__PROJECT_NAME__.tests)

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

(defun suppress-output (fn)
  "Call FN with stdout and stderr suppressed. Returns the result of FN."
  (let ((result nil))
    (with-output-to-string (*standard-output*)
      (with-output-to-string (*error-output*)
        (setf result (funcall fn))))
    result))

;;;; ============================================================
;;;; Master Test Suite
;;;; ============================================================

(def-suite __PROJECT_NAME__-tests
  :description "All tests for the web service")

;;;; ============================================================
;;;; Utils Module Tests
;;;; ============================================================

(def-suite utils-tests
  :description "Unit tests for utils module"
  :in __PROJECT_NAME__-tests)

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
  :description "Unit tests for core config module"
  :in __PROJECT_NAME__-tests)

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
    (is (= 3000 (suppress-output
                  (lambda () (parse-env-var-integer vars "PORT" 3000)))))))

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

(test read-env-file-directory-error
  "Test reading a directory path triggers error handler"
  (is-false (suppress-output
              (lambda () (read-env-file #p"/tmp/")))))

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
  :description "Unit tests for core server module"
  :in __PROJECT_NAME__-tests)

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

;;; --- stop-acceptor error handling ---

(test stop-acceptor-error-handling
  "Test that stop-acceptor handles errors gracefully"
  (let ((acceptor (make-acceptor '(:port 9998 :address "127.0.0.1"
                                   :document-root #p"./static/"
                                   :access-log-destination nil
                                   :message-log-destination nil))))
    (is-false (suppress-output (lambda () (stop-acceptor acceptor))))))

;;; --- stop-server edge cases ---

(test stop-server-when-not-running
  "Test stopping server when no server is running returns t"
  (let ((*acceptor* nil))
    (suppress-output (lambda () (is-true (stop-server))))))

;;; --- start-server error handling ---

(test start-server-with-invalid-port
  "Test that start-server handles port binding error gracefully"
  (let ((*acceptor* nil)
        (*config* '(:server (:port 1 :address "127.0.0.1"
                     :document-root #p"./static/"
                     :access-log-destination nil
                     :message-log-destination nil))))
    (is-false (suppress-output (lambda () (start-server))))))

;;;; ============================================================
;;;; Web Route Tests
;;;; ============================================================

(def-suite web-route-tests
  :description "Unit tests for web routes module"
  :in __PROJECT_NAME__-tests)

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
  :description "Unit tests for API routes module"
  :in __PROJECT_NAME__-tests)

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
  :description "Unit tests for main module"
  :in __PROJECT_NAME__-tests)

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

;;; --- load-dependencies ---

(test load-dependencies-success
  "Test that load-dependencies returns T when all deps are available"
  (is-true (suppress-output (lambda () (load-dependencies)))))

(test load-dependencies-failure
  "Test that load-dependencies returns NIL when a dep fails to load"
  (is-false (suppress-output
              (lambda ()
                (load-dependencies '(:nonexistent-system-that-does-not-exist-xyz))))))

;;; --- main function ---

(test main-function-success
  "Test that main starts the application successfully"
  (unwind-protect
      (let ((result (suppress-output (lambda () (main)))))
        (is-true result)
        (is (typep result 'hunchentoot:easy-acceptor)))
    (suppress-output (lambda () (stop-server)))))

(test main-function-dependency-failure
  "Test that main returns NIL when load-dependencies fails"
  (let ((original-fn (fdefinition '__PROJECT_NAME__::load-dependencies)))
    (unwind-protect
        (progn
          (setf (fdefinition '__PROJECT_NAME__::load-dependencies)
                (lambda (&optional deps) (declare (ignore deps)) nil))
          (is-false (suppress-output (lambda () (main)))))
      (setf (fdefinition '__PROJECT_NAME__::load-dependencies) original-fn))))

(test main-function-server-failure
  "Test that main returns NIL when server fails to start"
  (let ((*config* '(:server (:port 1 :address "127.0.0.1"
                     :document-root #p"./static/"
                     :access-log-destination nil
                     :message-log-destination nil))))
    (let ((result (suppress-output (lambda () (main)))))
      (is-false result))))

;;;; ============================================================
;;;; Integration Tests (HTTP)
;;;; ============================================================

(def-suite integration-tests
  :description "HTTP integration tests requiring a running server"
  :in __PROJECT_NAME__-tests)

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
     (suppress-output (lambda () (start-server 8089)))
     (is-true *acceptor*)
     (suppress-output (lambda () (stop-server))))))

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

;;; --- ensure-string utility ---

(test ensure-string-with-string-input
  "Test ensure-string returns string input unchanged"
  (is (equal "hello" (ensure-string "hello"))))

(test ensure-string-with-byte-array
  "Test ensure-string converts byte array to string"
  (let ((bytes (flexi-streams:string-to-octets "hello" :external-format :utf-8)))
    (is (equal "hello" (ensure-string bytes)))))

;;;; ============================================================
;;;; Test Runner
;;;; ============================================================

(defun run-tests ()
  (format t "Running tests for the web service...~%")
  (run! '__PROJECT_NAME__-tests)
  (format t "Tests completed.~%"))
TESTEOF

sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/tests/test-suite.lisp"

echo ""
echo "=========================================="
echo "Web Service Project '$PROJECT_NAME' created successfully!"
echo ""
echo "Next steps:"
echo "  1. cd $PROJECT_NAME"
echo "  2. sbcl --load setup-quicklisp.lisp --quit"
echo "  3. ./run-server.sh"
echo "  4. curl http://localhost:8080/api/health"
echo ""
echo "Happy hacking!"
