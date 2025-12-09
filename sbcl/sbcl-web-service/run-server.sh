#!/bin/bash

# Check if Quicklisp is installed, if not, install it
if [ ! -f "quicklisp/setup.lisp" ]; then
    echo "Quicklisp not found. Installing..."
    sbcl --load setup-quicklisp.lisp --quit
    if [ $? -ne 0 ]; then
        echo "Failed to install Quicklisp. Exiting."
        exit 1
    fi
    echo "Quicklisp installed successfully."
fi

# Create a startup script for SBCL
cat > run-server.lisp << 'EOF'
(load "initialize.lisp")
(format t "~%Starting server in the background...~%")
(sb-thread:make-thread #'sbcl-web-service:main)
(format t "Server should now be running. Test with: curl http://localhost:8080/api/health~%")
(format t "Press Ctrl+C to stop the server~%")
(loop (sleep 3600))
EOF

# Run SBCL with the startup script
sbcl --load run-server.lisp
