#!/bin/bash

# Test runner script for sbcl-cli-project

echo "Running tests for sbcl-cli-project..."

sbcl --noinform \
     --eval "(ql:quickload :sbcl-cli-project-tests :silent t)" \
     --eval "(asdf:test-system :sbcl-cli-project-tests)" \
     --eval "(quit)"
