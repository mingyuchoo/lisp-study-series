#!/bin/bash

# Run the SBCL web service in the background
sbcl --load start.lisp --eval '(sb-thread:make-thread (lambda () (sleep 3600)))' &

# Print a message
echo "Server started in the background. Use 'curl http://localhost:8080/api/health' to test it."
echo "To stop the server, find its process ID with 'ps aux | grep sbcl' and kill it."
