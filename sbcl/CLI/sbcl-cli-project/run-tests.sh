#!/bin/bash

# Test runner script for sbcl-cli-project

echo "Running tests for sbcl-cli-project..."

sbcl --noinform \
     --load "quicklisp/setup.lisp" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(push (truename \"./tests/\") asdf:*central-registry*)" \
     --eval "(ql:quickload :sbcl-cli-project-tests :silent t)" \
     --eval "(asdf:test-system :sbcl-cli-project-tests)" \
     --eval "(quit)"
