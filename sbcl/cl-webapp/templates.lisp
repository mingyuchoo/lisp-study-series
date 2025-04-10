;;;; templates.lisp
;;;; Template definitions for the Common Lisp web application

(in-package :cl-webapp)

;; Define a macro for HTML generation
(defmacro with-html-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title ,title)
       (:link :rel "stylesheet" :href "/css/main.css")
       (:script :src "/js/app.js"))
      (:body
       (:header
        (:nav
         (:ul
          (:li (:a :href "/" "Home"))
          (:li (:a :href "/about" "About"))
          (:li (:a :href "/features" "Features"))
          (:li (:a :href "/api" "API")))))
       (:main
        (:div :class "container"
              ,@body))
       (:footer
        (:div :class "container"
              (:p "Common Lisp Web Application Demo © 2023")))))))

;; Define actual page templates
(defun home-page-template ()
  (with-html-page (:title "Common Lisp Web App")
    (:h1 "Welcome to the Common Lisp Web Application")
    (:p "This is a full-featured web application built with Common Lisp and Hunchentoot.")
    (:div :class "features"
          (:h2 "Features")
          (:ul
           (:li "Hunchentoot Web Server")
           (:li "HTML Generation with CL-WHO")
           (:li "CSS Generation with CL-CSS")
           (:li "JavaScript with Parenscript")
           (:li "JSON API Endpoints")))))

(defun about-page-template ()
  (with-html-page (:title "About | Common Lisp Web App")
    (:h1 "About This Project")
    (:p "This application demonstrates how to build a complete web application in Common Lisp.")
    (:h2 "Technical Stack")
    (:ul 
     (:li (:strong "Web Server: ") "Hunchentoot")
     (:li (:strong "HTML Generation: ") "CL-WHO and Spinneret")
     (:li (:strong "JavaScript Generation: ") "Parenscript")
     (:li (:strong "CSS Generation: ") "CL-CSS"))))

(defun features-page-template ()
  (with-html-page (:title "Features | Common Lisp Web App")
    (:h1 "Features")
    (:div :class "feature-card"
          (:h3 "High-Performance Web Server")
          (:p "Hunchentoot is a robust, high-performance web server written in Common Lisp."))
    (:div :class "feature-card"
          (:h3 "Lisp-Generated Frontend")
          (:p "Generate HTML, CSS, and JavaScript directly from Lisp code for seamless integration."))
    (:div :class "feature-card"
          (:h3 "API Endpoints")
          (:p "Support for JSON API endpoints for building modern applications."))))

(defun not-found-template ()
  (with-html-page (:title "404 Not Found | Common Lisp Web App")
    (:h1 "404 - Page Not Found")
    (:p "The page you're looking for doesn't exist.")
    (:a :href "/" "Go back to the home page")))

(defun api-info-template ()
  (with-html-page (:title "API Info | Common Lisp Web App")
    (:h1 "API Information")
    (:p "This application provides a JSON API for programmatic access.")
    (:h2 "Endpoints")
    (:ul
     (:li (:code "GET /api/info") " - Basic info about the application")
     (:li (:code "GET /api/status") " - System status information"))
    (:h2 "Example Usage")
    (:pre :class "code"
          (:code "curl -X GET http://localhost:5000/api/info"))))
