#!/bin/bash

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
