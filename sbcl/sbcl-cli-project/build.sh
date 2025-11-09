#!/usr/bin/env sh

# Store the first argument
PROJECT_NAME=$1

# Check the first argument
if [ -z "$PROJECT_NAME" ]; then
    echo "Usage: $0 <project-name>"
    exit 1
fi

# Run build command
sbcl --eval "(require :asdf)"                                                    \
     --eval "(push (uiop:getcwd) asdf:*central-registry*)"                       \
     --eval "(asdf:load-system :$PROJECT_NAME)"                                  \
     --eval "(asdf:make :$PROJECT_NAME)"                                         \
     --eval "(quit)"
