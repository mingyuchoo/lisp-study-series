#!/bin/bash
sbcl --eval "(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))" \
     --eval "(ql:quickload '(:fiveam :drakma))" \
     --eval "(ql:quickload :sbcl-web-service/tests)" \
     --eval "(sbcl-web-service.tests:run-tests)" \
     --quit