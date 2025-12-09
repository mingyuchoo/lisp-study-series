#!/usr/bin/env sh

# Get the current directory name as project name
PROJECT_NAME=$(basename "$(pwd)")

# Run build command
sbcl --eval "(require :asdf)"                                                    \
     --eval "(push (uiop:getcwd) asdf:*central-registry*)"                       \
     --eval "(asdf:load-system :$PROJECT_NAME)"                                  \
     --eval "(asdf:make :$PROJECT_NAME)"                                         \
     --eval "(quit)"
