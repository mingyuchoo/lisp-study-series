#!/usr/bin/env bash
set -e

PROJECT_NAME=$(basename "$(pwd)")

sbcl --noinform \
     --load "quicklisp/setup.lisp" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(asdf:load-system :$PROJECT_NAME)" \
     --eval "(asdf:make :$PROJECT_NAME)" \
     --eval "(quit)"
