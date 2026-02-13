#!/usr/bin/env bash
set -e

PROJECT_NAME=$(basename "$(pwd)")

echo "Running tests for $PROJECT_NAME..."

sbcl --noinform \
     --load "quicklisp/setup.lisp" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(push (truename \"./tests/\") asdf:*central-registry*)" \
     --eval "(ql:quickload :${PROJECT_NAME}-tests :silent t)" \
     --eval "(asdf:test-system :${PROJECT_NAME}-tests)" \
     --eval "(quit)"
