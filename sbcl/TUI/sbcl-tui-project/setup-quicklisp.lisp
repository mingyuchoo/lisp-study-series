;;;; setup-quicklisp.lisp
;;;; Script to install and set up Quicklisp

(format t "~%Setting up Quicklisp for TUI Application...~%")

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
               '(:croatoan))
      (format t "All packages installed successfully.~%"))
  (error (e)
    (format t "Error installing packages: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Quicklisp setup complete.~%")

