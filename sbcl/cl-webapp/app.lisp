;;;; app.lisp
;;;; Main application code for the Common Lisp web application

(in-package :cl-webapp)

;; Define configuration constants
(defvar *server* nil 
  "The Hunchentoot server instance")

(defparameter *server-port* 5000 
  "The port on which the server will listen")

(defparameter *server-address* "0.0.0.0"
  "The address on which the server will listen")

(defparameter *application-root* 
  (asdf:system-source-directory :cl-webapp)
  "The root directory of the application")

(defparameter *static-directory* 
  (merge-pathnames #p"static/" *application-root*)
  "Directory containing static assets")

;; Set up parenscript for JavaScript generation
(setf parenscript:*js-string-delimiter* #\")

;; Generate CSS with cl-css
(defun generate-dynamic-css ()
  (cl-css:css
   `((.dynamic-content
      :background-color "#f9f9f9"
      :padding "20px"
      :border-radius "5px"
      :margin-top "20px"
      :box-shadow "0 1px 3px rgba(0,0,0,0.12)")
     (.api-section
      :background-color "#e9f5ff"
      :border-left "5px solid #4a86e8"
      :padding "15px"
      :margin "20px 0"))))

;; Generate JavaScript with parenscript
(defun generate-dynamic-js ()
  (parenscript:ps
    (defun show-server-time ()
      (let ((time-display (chain document (get-element-by-id "server-time"))))
        (when time-display
          (setf (@ time-display inner-text) 
                (new (-date))))))
    
    (defun init-dynamic-features ()
      (show-server-time)
      (setf *timer* (chain window (set-interval show-server-time 1000))))
    
    (chain window (add-event-listener "load" init-dynamic-features))))

;; Route handlers
(defun handle-home-page ()
  (home-page-template))

(defun handle-about-page ()
  (about-page-template))

(defun handle-features-page ()
  (features-page-template))

(defun handle-api-info-page ()
  (api-info-template))

;; Dynamic asset handlers
(defun handle-dynamic-css ()
  (setf (content-type*) "text/css")
  (generate-dynamic-css))

(defun handle-dynamic-js ()
  (setf (content-type*) "application/javascript")
  (generate-dynamic-js))

;; API handlers
(defun handle-api-info ()
  (setf (content-type*) "application/json")
  (format nil "{\"name\":\"Common Lisp Web Application\",\"version\":\"0.1.0\",\"description\":\"A full-featured web application in Common Lisp\",\"endpoints\":[\"/api/info\",\"/api/status\"]}"))

(defun handle-api-status ()
  (setf (content-type*) "application/json")
  (format nil "{\"status\":\"running\",\"time\":~A,\"system\":\"SBCL\",\"lisp-implementation\":\"~A\",\"lisp-version\":\"~A\"}" 
          (get-universal-time)
          (lisp-implementation-type)
          (lisp-implementation-version)))

;; Set up dispatchers
(defun initialize-dispatchers ()
  (list
   ;; Main page routes
   (create-prefix-dispatcher "/" 'handle-home-page)
   (create-prefix-dispatcher "/about" 'handle-about-page)
   (create-prefix-dispatcher "/features" 'handle-features-page)
   (create-prefix-dispatcher "/api" 'handle-api-info-page)
   
   ;; API endpoints
   (create-prefix-dispatcher "/api/info" 'handle-api-info)
   (create-prefix-dispatcher "/api/status" 'handle-api-status)
   
   ;; Dynamic CSS and JS
   (create-prefix-dispatcher "/css/dynamic.css" 'handle-dynamic-css)
   (create-prefix-dispatcher "/js/dynamic.js" 'handle-dynamic-js)
   
   ;; Static file dispatcher (must be last)
   (create-folder-dispatcher-and-handler 
    "/css/" (merge-pathnames #p"css/" *static-directory*))
   (create-folder-dispatcher-and-handler 
    "/js/" (merge-pathnames #p"js/" *static-directory*))))

;; 404 handler
(defmethod acceptor-dispatch-request :around ((acceptor acceptor) request)
  (handler-case
      (call-next-method)
    (handler-not-found ()
      (setf (return-code*) 404)
      (not-found-template))))

;; Server management functions
(defun start-server ()
  "Start the Hunchentoot web server."
  (format t "Starting server on ~A:~A~%" *server-address* *server-port*)
  (setf *server*
        (start (make-instance 'easy-acceptor
                                        :port *server-port*
                                        :address *server-address*
                                        :document-root *static-directory*)))
  ;; Register dispatchers
  (setf *dispatch-table* (initialize-dispatchers))
  (format t "Server started successfully~%"))

(defun stop-server ()
  "Stop the Hunchentoot web server."
  (when *server*
    (format t "Stopping server~%")
    (stop *server*)
    (setf *server* nil)
    (format t "Server stopped~%")))

(defun restart-server ()
  "Restart the Hunchentoot web server."
  (format t "Restarting server~%")
  (stop-server)
  (start-server))

;; Print startup message
(defun print-startup-banner ()
  (format t "~%~%  Common Lisp Web Application~%")
  (format t "  =========================~%")
  (format t "  Version: 0.1.0~%")
  (format t "  Server: Hunchentoot~%")
  (format t "  Address: ~A:~A~%~%" *server-address* *server-port*))
