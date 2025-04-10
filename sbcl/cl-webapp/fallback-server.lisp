;;;; fallback-server.lisp
;;;; A simple HTTP server implementation for when Hunchentoot can't be loaded

(format t "~%Using fallback HTTP server implementation~%")

(defpackage :cl-webapp
  (:use :cl)
  (:export :start-server
           :stop-server
           :restart-server
           :*server*
           :*application-root*
           :print-startup-banner))

(in-package :cl-webapp)

;; Define configuration constants
(defvar *server* nil 
  "The server socket instance")

(defparameter *server-port* 5000
  "The port on which the server will listen")

(defparameter *server-address* #(0 0 0 0)
  "The address on which the server will listen")

(defparameter *application-root* 
  (truename ".")
  "The root directory of the application")

(defparameter *static-directory* 
  (merge-pathnames #p"static/" *application-root*)
  "Directory containing static assets")

;; Simple HTML generator for our static pages
(defun html-page (title body)
  (format nil "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>~A</title>
  <style>
    body { font-family: Arial, sans-serif; line-height: 1.6; margin: 0; padding: 0; }
    .container { max-width: 1140px; margin: 0 auto; padding: 0 15px; }
    header { background-color: #4a86e8; color: white; padding: 1rem 0; }
    nav ul { display: flex; list-style: none; justify-content: center; }
    nav li { margin: 0 15px; }
    nav a { color: white; text-decoration: none; }
    main { padding: 2rem 0; }
    footer { background-color: #f1f1f1; padding: 1rem 0; text-align: center; }
    h1 { color: #4a86e8; }
    .feature-card { background-color: #f9f9f9; border-radius: 5px; padding: 1rem; margin-bottom: 1rem; }
  </style>
</head>
<body>
  <header>
    <nav>
      <ul>
        <li><a href=\"/\">Home</a></li>
        <li><a href=\"/about\">About</a></li>
        <li><a href=\"/features\">Features</a></li>
        <li><a href=\"/api\">API</a></li>
      </ul>
    </nav>
  </header>
  <main>
    <div class=\"container\">
      ~A
    </div>
  </main>
  <footer>
    <div class=\"container\">
      <p>Common Lisp Web Application © 2023</p>
    </div>
  </footer>
</body>
</html>" title body))

;; Actual page content
(defun home-page ()
  (html-page "Common Lisp Web App" 
             "<h1>Welcome to the Common Lisp Web Application</h1>
              <p>This is a fallback implementation of the web server due to Hunchentoot loading issues.</p>
              <div>
                <h2>Features</h2>
                <ul>
                  <li>Simple HTTP Server Implementation</li>
                  <li>Static Asset Serving</li>
                  <li>Multiple Routes</li>
                  <li>API Endpoints</li>
                </ul>
              </div>"))

(defun about-page ()
  (html-page "About | Common Lisp Web App"
             "<h1>About This Project</h1>
              <p>This application normally uses Hunchentoot as its web server, but has fallen back to a simpler implementation.</p>
              <h2>Details</h2>
              <p>The fallback server is implemented using SB-BSD-SOCKETS and basic socket programming.</p>"))

(defun features-page ()
  (html-page "Features | Common Lisp Web App"
             "<h1>Features</h1>
              <div class=\"feature-card\">
                <h3>Resilient Design</h3>
                <p>Falls back to a simpler implementation when dependencies can't be loaded.</p>
              </div>
              <div class=\"feature-card\">
                <h3>Static Asset Serving</h3>
                <p>CSS and JavaScript files are still served correctly.</p>
              </div>
              <div class=\"feature-card\">
                <h3>API Endpoints</h3>
                <p>Basic JSON API endpoints still function.</p>
              </div>"))

(defun api-info-page ()
  (html-page "API Info | Common Lisp Web App"
             "<h1>API Information</h1>
              <p>The fallback server still supports basic API endpoints.</p>
              <h2>Endpoints</h2>
              <ul>
                <li><code>GET /api/info</code> - Basic info about the application</li>
                <li><code>GET /api/status</code> - System status information</li>
              </ul>
              <h2>Example Usage</h2>
              <pre><code>curl -X GET http://localhost:5000/api/info</code></pre>"))

(defun not-found-page ()
  (html-page "404 Not Found | Common Lisp Web App"
             "<h1>404 - Page Not Found</h1>
              <p>The page you're looking for doesn't exist.</p>
              <a href=\"/\">Go back to the home page</a>"))

(defun error-page ()
  (html-page "Error | Common Lisp Web App"
             "<h1>Server Error</h1>
              <p>An error occurred while processing your request.</p>
              <a href=\"/\">Go back to the home page</a>"))

;; API responses
(defun api-info-response ()
  "{\"name\":\"Common Lisp Web Application\",\"version\":\"0.1.0\",\"description\":\"A fallback implementation of the web server\",\"endpoints\":[\"/api/info\",\"/api/status\"]}")

(defun api-status-response ()
  (format nil "{\"status\":\"running\",\"time\":~A,\"system\":\"SBCL\",\"lisp-implementation\":\"~A\",\"lisp-version\":\"~A\",\"mode\":\"fallback\"}" 
          (get-universal-time)
          (lisp-implementation-type)
          (lisp-implementation-version)))

;; Server functions
(defun http-response (status content-type body)
  (let* ((content-length (length body))
         (response (format nil "HTTP/1.1 ~A~%Content-Type: ~A~%Content-Length: ~D~%Connection: close~%~%~A"
                           status content-type content-length body))
         (response-array (make-array (length response) 
                                    :element-type '(unsigned-byte 8))))
    ;; Convert string to byte array
    (loop for i from 0 below (length response)
          do (setf (aref response-array i) (char-code (char response i))))
    response-array))

;; Basic socket server
(defun start-server-thread ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                              :type :stream 
                              :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket *server-address* *server-port*)
    (sb-bsd-sockets:socket-listen socket 5)
    (setf *server* socket)
    
    (format t "Server listening on port ~D~%" *server-port*)
    
    (loop
      (format t "Waiting for connection...~%")
      (handler-case
          (let ((client (sb-bsd-sockets:socket-accept socket)))
            (format t "Connection accepted~%")
            (handler-case
                (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                      (bytes-read 0))
                  (setf bytes-read 
                        (sb-bsd-sockets:socket-receive client buffer nil))
                  
                  (let* ((request-string (handler-case
                                             (map 'string 
                                                  (lambda (byte) 
                                                    (if (< byte 128) (code-char byte) #\Space))
                                                  (subseq buffer 0 bytes-read))
                                           (error (e) 
                                             (format t "Error converting request: ~A~%" e)
                                             "")))
                         (path "/"))
                    
                    ;; Extract path from request
                    (when (> (length request-string) 0)
                      (let* ((first-line-end (position #\Return request-string))
                             (first-line (if first-line-end 
                                            (subseq request-string 0 first-line-end)
                                            request-string))
                             (parts (loop for i = 0 then (1+ j)
                                          as j = (position #\Space first-line :start i)
                                          collect (subseq first-line i j)
                                          while j)))
                        (when (>= (length parts) 2)
                          (setf path (second parts)))))
                    
                    (format t "Request for path: ~A~%" path)
                    
                    ;; Generate response based on path
                    (let ((response-bytes 
                           (cond
                             ;; Main pages
                             ((string= path "/") 
                              (http-response "200 OK" "text/html; charset=utf-8" 
                                            (home-page)))
                             
                             ((string= path "/about") 
                              (http-response "200 OK" "text/html; charset=utf-8" 
                                            (about-page)))
                             
                             ((string= path "/features") 
                              (http-response "200 OK" "text/html; charset=utf-8" 
                                            (features-page)))
                             
                             ((string= path "/api") 
                              (http-response "200 OK" "text/html; charset=utf-8" 
                                            (api-info-page)))
                             
                             ;; API endpoints
                             ((string= path "/api/info") 
                              (http-response "200 OK" "application/json" 
                                            (api-info-response)))
                             
                             ((string= path "/api/status") 
                              (http-response "200 OK" "application/json" 
                                            (api-status-response)))
                             
                             ;; If no route matches, return 404
                             (t (http-response "404 Not Found" "text/html; charset=utf-8" 
                                              (not-found-page))))))
                      
                      ;; Send response
                      (sb-bsd-sockets:socket-send client response-bytes 
                                                  (length response-bytes)))))
              (error (e)
                (format t "Error processing request: ~A~%" e)))
            
            ;; Close client connection
            (handler-case
                (sb-bsd-sockets:socket-close client)
              (error (e)
                (format t "Error closing client socket: ~A~%" e))))
        (error (e)
          (format t "Error accepting connection: ~A~%" e))))))

;; Public interface
(defun start-server ()
  (format t "Starting fallback server on port ~D~%" *server-port*)
  (sb-thread:make-thread #'start-server-thread 
                         :name "Web Server Thread"))

(defun stop-server ()
  (when *server*
    (format t "Stopping server~%")
    (handler-case
        (sb-bsd-sockets:socket-close *server*)
      (error (e)
        (format t "Error closing server socket: ~A~%" e)))
    (setf *server* nil)))

(defun restart-server ()
  (stop-server)
  (start-server))

(defun print-startup-banner ()
  (format t "~%~%  Common Lisp Web Application (FALLBACK SERVER)~%")
  (format t "  =======================================~%")
  (format t "  Version: 0.1.0~%")
  (format t "  Server: Fallback Socket Server~%")
  (format t "  Address: ~A:~A~%~%" *server-address* *server-port*))
