;;;; setup-quicklisp.lisp
;;;; Script to install and set up Quicklisp

(format t "~%Setting up Quicklisp for sbcl-cli-project...~%")

;; Check if Quicklisp is already installed in project directory
(if (probe-file "quicklisp/setup.lisp")
    (format t "Quicklisp already installed in project directory.~%")
    (progn
      (format t "Downloading and installing Quicklisp...~%")
      
      ;; Download Quicklisp
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
               '(:fiveam))
      (format t "All packages installed successfully.~%"))
  (error (e)
    (format t "Error installing packages: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Quicklisp setup complete.~%")
