#!/usr/bin/env bash
set -e

# Install Quicklisp locally if not present
if [ ! -f "quicklisp/setup.lisp" ]; then
  echo "Quicklisp not found. Running setup..."
  sbcl --noinform --non-interactive --load "setup-quicklisp.lisp"
fi

PROJECT_NAME=$(basename "$(pwd)")

sbcl --noinform \
     --load "quicklisp/setup.lisp" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(handler-bind ((warning #'muffle-warning)) (asdf:load-system :$PROJECT_NAME))" \
     --eval "(handler-bind ((warning #'muffle-warning)) (asdf:make :$PROJECT_NAME))" \
     --eval "(quit)"
