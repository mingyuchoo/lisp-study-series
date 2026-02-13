#!/bin/bash
cd "$(dirname "$0")"
sbcl --eval "(require :asdf)" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(load (merge-pathnames \"quicklisp/setup.lisp\" (truename \".\")))" \
     --eval "(ql:quickload '(:fiveam :drakma) :silent t)" \
     --eval "(ql:quickload :sbcl-web-service/tests :silent t)" \
     --eval "(sbcl-web-service.tests:run-tests)" \
     --quit
