#!/usr/bin/env bash
set -e

PROJECT_NAME=$(basename "$(pwd)")
REPORT_DIR="coverage-report"

echo "Running coverage for $PROJECT_NAME..."

rm -rf "$REPORT_DIR"
mkdir -p "$REPORT_DIR"

sbcl --noinform \
     --load "quicklisp/setup.lisp" \
     --eval "(require :sb-cover)" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(push (truename \"./tests/\") asdf:*central-registry*)" \
     --eval "(declaim (optimize sb-cover:store-coverage-data))" \
     --eval "(asdf:load-system :$PROJECT_NAME :force t)" \
     --eval "(ql:quickload :${PROJECT_NAME}-tests :silent t)" \
     --eval "(asdf:test-system :${PROJECT_NAME}-tests)" \
     --eval "(sb-cover:report \"$REPORT_DIR/\")" \
     --eval "(format t \"~%Coverage report: $(pwd)/$REPORT_DIR/cover-index.html~%\")" \
     --eval "(declaim (optimize (sb-cover:store-coverage-data 0)))" \
     --eval "(sb-cover:reset-coverage)" \
     --eval "(quit)"
