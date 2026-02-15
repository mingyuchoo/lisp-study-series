#!/bin/bash

# SBCL CLI Project Generator
# Usage: ./sbcl-cli-gen.sh <project-name>

set -e

# Check if project name is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <project-name>"
    echo "Example: $0 my-lisp-project"
    exit 1
fi

PROJECT_NAME="$1"

# Check if directory already exists
if [ -d "$PROJECT_NAME" ]; then
    echo "Error: Directory '$PROJECT_NAME' already exists"
    exit 1
fi

echo "Creating SBCL CLI project: $PROJECT_NAME"
echo "========================================"

# Create project directory structure
mkdir -p "$PROJECT_NAME/src"
mkdir -p "$PROJECT_NAME/tests"

# Create .gitignore
cat > "$PROJECT_NAME/.gitignore" << 'EOF'
*.FASL
*.fasl
*.lisp-temp
*.dfsl
*.pfsl
*.d64fsl
*.p64fsl
*.lx64fsl
*.lx32fsl
*.dx64fsl
*.dx32fsl
*.fx64fsl
*.fx32fsl
*.sx64fsl
*.sx32fsl
*.wx64fsl
*.wx32fsl
*.abcl
*.x86f
*~
.#*
# Ignore the quicklisp directory
**/quicklisp/
# Coverage
**/coverage-report/
EOF

# Create .tool-versions
cat > "$PROJECT_NAME/.tool-versions" << 'EOF'
sbcl 2.6.0

EOF

# Copy quicklisp.lisp from current directory
if [ -f "quicklisp.lisp" ]; then
    cp "quicklisp.lisp" "$PROJECT_NAME/quicklisp.lisp"
    echo "✓ Copied quicklisp.lisp"
else
    echo "Warning: quicklisp.lisp not found in current directory"
    echo "You may need to download it manually from https://beta.quicklisp.org/quicklisp.lisp"
fi

# Create README.md
cat > "$PROJECT_NAME/README.md" << EOF
# $PROJECT_NAME

## 1. Setup Quicklisp

First, install Quicklisp in the project directory:

\`\`\`bash
cd $PROJECT_NAME
sbcl --script setup-quicklisp.lisp
\`\`\`

## 2. Load and run your project

\`\`\`bash
sbcl

CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (ql:quickload :$PROJECT_NAME)
CL-USER> (in-package :$PROJECT_NAME)
$PROJECT_NAME> (main)
Hello, World!
\`\`\`

## 3. Build binary

\`\`\`bash
./run-build.sh
\`\`\`

This will create an executable binary named \`$PROJECT_NAME\`.

## 4. Run tests

\`\`\`bash
./run-tests.sh
\`\`\`

## Project Structure

\`\`\`
$PROJECT_NAME/
├── .gitignore
├── .tool-versions
├── README.md
├── run-build.sh
├── quicklisp.lisp
├── run-tests.sh
├── setup-quicklisp.lisp
├── $PROJECT_NAME.asd
├── src/
│   └── main.lisp
└── tests/
    ├── README.md
    ├── main-tests.lisp
    └── $PROJECT_NAME-tests.asd
\`\`\`

## References

- [Quicklisp](https://www.quicklisp.org/)
- [SBCL](http://www.sbcl.org/)
EOF

# Create setup-quicklisp.lisp
cat > "$PROJECT_NAME/setup-quicklisp.lisp" << 'EOF'
;;;; setup-quicklisp.lisp
;;;; Script to install and set up Quicklisp

(format t "~%Setting up Quicklisp for Common Lisp Web Application...~%")

;; Check if Quicklisp is already installed in project directory
(if (probe-file "quicklisp/setup.lisp")
    (format t "Quicklisp already installed in project directory.~%")
    (progn
      (format t "Downloading and installing Quicklisp...~%")
      
      ;; Download Quicklisp if not present
      (unless (probe-file "quicklisp.lisp")
        (format t "Downloading quicklisp.lisp...~%")
        (let ((quicklisp-url "https://beta.quicklisp.org/quicklisp.lisp"))
          #+sbcl
          (sb-ext:run-program "curl" 
                              (list "-o" "quicklisp.lisp" "-L" quicklisp-url)
                              :search t
                              :output t
                              :error t)
          (unless (probe-file "quicklisp.lisp")
            (format t "Error: Failed to download quicklisp.lisp~%")
            (sb-ext:exit :code 1))))
      
      ;; Load Quicklisp
      (load "quicklisp.lisp")
      
      ;; Install Quicklisp into the project directory
      (handler-case
          (progn
            (funcall (intern "INSTALL" (find-package "QUICKLISP-QUICKSTART"))
                     :path (merge-pathnames "quicklisp/" (truename ".")))
            (format t "Quicklisp installed successfully.~%"))
        (error (e)
          (format t "Error installing Quicklisp: ~A~%" e)
          (sb-ext:exit :code 1)))))

;; Load Quicklisp from project directory
(format t "Loading Quicklisp from project directory...~%")
(handler-case
    (load (merge-pathnames "quicklisp/setup.lisp" (truename ".")))
  (error (e)
    (format t "Error loading Quicklisp: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Install required packages
(format t "Installing required packages...~%")
(handler-case
    (progn
      (funcall (intern "QUICKLOAD" (find-package "QL")) 
               '(:hunchentoot :cl-who :parenscript :cl-css :spinneret))
      (format t "All packages installed successfully.~%"))
  (error (e)
    (format t "Error installing packages: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Quicklisp setup complete.~%")

EOF

# Create project .asd file
cat > "$PROJECT_NAME/$PROJECT_NAME.asd" << EOF
(defsystem "$PROJECT_NAME"
  :version "0.0.1"
  :author ""
  :license "BSD-3-Clause license"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Project for Lisp"
  :build-operation "program-op"
  :build-pathname "$PROJECT_NAME"
  :entry-point "$PROJECT_NAME:main")

EOF

# Create src/main.lisp
cat > "$PROJECT_NAME/src/main.lisp" << EOF
(defpackage $PROJECT_NAME
  (:use :cl)
  (:export :main))
  
(in-package :$PROJECT_NAME)

;; Main function
(defun main ()
  (format t "Hello, World!~%"))

EOF

# Create run-build.sh
cat > "$PROJECT_NAME/run-build.sh" << EOF
#!/usr/bin/env sh

# Install Quicklisp
sbcl --script setup-quicklisp.lisp

# Run build command
sbcl --eval "(require :asdf)" \\
     --eval "(push (uiop:getcwd) asdf:*central-registry*)" \\
     --eval "(asdf:load-system :$PROJECT_NAME)" \\
     --eval "(asdf:make :$PROJECT_NAME)" \\
     --eval "(quit)"
EOF

chmod +x "$PROJECT_NAME/run-build.sh"

# Create run-tests.sh
cat > "$PROJECT_NAME/run-tests.sh" << EOF
#!/bin/bash

# Test runner script for sbcl-cli-project
echo "Running tests for sbcl-cli-project..."

sbcl --noinform \\
     --load "quicklisp/setup.lisp" \\
     --eval "(push (truename \".\") asdf:*central-registry*)" \\
     --eval "(push (truename \"./tests/\") asdf:*central-registry*)" \\
     --eval "(ql:quickload :$PROJECT_NAME-tests :silent t)" \\
     --eval "(asdf:test-system :$PROJECT_NAME-tests)" \\
     --eval "(quit)"
EOF

chmod +x "$PROJECT_NAME/run-tests.sh"

# Create run-coverage.sh
cat > "$PROJECT_NAME/run-coverage.sh" << EOF
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
EOF

chmod +x "$PROJECT_NAME/run-coverage.sh"

# Create tests/README.md
cat > "$PROJECT_NAME/tests/README.md" << 'EOF'
# Tests

This directory contains test files for the project.

## Running Tests

```bash
./run-tests.sh
```

## Writing Tests

Add your test files here and update the test system definition in `*-tests.asd`.
EOF

# Create tests/main-tests.lisp
cat > "$PROJECT_NAME/tests/main-tests.lisp" << EOF
(defpackage $PROJECT_NAME-tests
  (:use :cl :$PROJECT_NAME)
  (:export :run-tests))

(in-package :$PROJECT_NAME-tests)

;; Add your tests here
(defun run-tests ()
  (format t "Running tests...~%")
  (format t "All tests passed!~%"))

EOF

# Create tests system definition
cat > "$PROJECT_NAME/tests/$PROJECT_NAME-tests.asd" << EOF
(defsystem "$PROJECT_NAME-tests"
  :version "0.0.1"
  :author ""
  :license "BSD-3-Clause license"
  :depends-on (:$PROJECT_NAME)
  :components ((:file "main-tests"))
  :description "Test system for $PROJECT_NAME"
  :perform (test-op (op c) 
                    (symbol-call :$PROJECT_NAME-tests :run-tests)))

EOF

echo ""
echo "========================================"
echo "✓ Project '$PROJECT_NAME' created successfully!"
echo ""
echo "Next steps:"
echo "  1. cd $PROJECT_NAME"
echo "  2. sbcl --script setup-quicklisp.lisp"
echo "  3. ./run-build.sh"
echo "  4. ./$PROJECT_NAME"
echo ""
echo "Happy hacking!"
