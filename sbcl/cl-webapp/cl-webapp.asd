;;;; cl-webapp.asd
;;;; System definition for the Common Lisp web application

(asdf:defsystem #:cl-webapp
  :description "A full-featured web application in Common Lisp"
  :author "Replit User"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:hunchentoot     ; Web server
               #:cl-who          ; HTML generation
               #:spinneret       ; HTML5 generator
               #:parenscript     ; JavaScript generation
               #:cl-css)         ; CSS generation
  :components ((:file "package")
               (:file "templates")
               (:file "app")))
