;; Load Quicklisp first - this must be done before anything else
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init))
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed. Please install Quicklisp first.")))

;; Load required packages
(ql:quickload '(:hunchentoot :cl-json :alexandria :cl-ppcre :cl-utilities))

;; Main function to start the application
(defun sbcl-web-service:main ()
  (format t "~%Starting SBCL Web Service Application~%")
  (sbcl-web-service:start-server))