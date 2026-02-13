#!/bin/bash

# SBCL TUI Project Generator (using croatoan)
# Usage: ./gen-tui.sh <project-name>

set -e

# Check if project name is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <project-name>"
    echo "Example: $0 my-tui-app"
    exit 1
fi

PROJECT_NAME="$1"

# Check if directory already exists
if [ -d "$PROJECT_NAME" ]; then
    echo "Error: Directory '$PROJECT_NAME' already exists"
    exit 1
fi

echo "Creating SBCL TUI project: $PROJECT_NAME"
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

A TUI (Terminal User Interface) application built with Common Lisp and croatoan.

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- ncurses 6.x development libraries

\`\`\`bash
# Ubuntu/Debian
sudo apt install libncursesw6 libncurses-dev

# Fedora/RHEL
sudo dnf install ncurses-devel

# macOS
brew install ncurses
\`\`\`

## 1. Setup Quicklisp

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
│   ├── package.lisp
│   ├── ui.lisp
│   └── main.lisp
└── tests/
    ├── README.md
    ├── main-tests.lisp
    └── $PROJECT_NAME-tests.asd
\`\`\`

## Key Bindings

| Key       | Action             |
|-----------|--------------------|
| ↑ / k     | Move up            |
| ↓ / j     | Move down          |
| Enter     | Select item        |
| q         | Quit               |

## References

- [croatoan](https://codeberg.org/McParen/croatoan) - Common Lisp ncurses bindings
- [Quicklisp](https://www.quicklisp.org/)
- [SBCL](http://www.sbcl.org/)
EOF

# Create setup-quicklisp.lisp
cat > "$PROJECT_NAME/setup-quicklisp.lisp" << 'EOF'
;;;; setup-quicklisp.lisp
;;;; Script to install and set up Quicklisp

(format t "~%Setting up Quicklisp for TUI Application...~%")

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
               '(:croatoan))
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
  :depends-on ("croatoan")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "ui")
                 (:file "main"))))
  :description "A TUI Application built with croatoan"
  :build-operation "program-op"
  :build-pathname "$PROJECT_NAME"
  :entry-point "$PROJECT_NAME:main")

EOF

# Create src/package.lisp
cat > "$PROJECT_NAME/src/package.lisp" << EOF
(defpackage $PROJECT_NAME
  (:use :cl :croatoan)
  (:export :main))

EOF

# Create src/ui.lisp
cat > "$PROJECT_NAME/src/ui.lisp" << EOF
(in-package :$PROJECT_NAME)

;;; ============================================================
;;; Pure functions - UI rendering helpers
;;; ============================================================

(defun make-padded-string (text total-width &optional (pad-char #\Space))
  "Create a string of TOTAL-WIDTH padded with PAD-CHAR after TEXT."
  (let* ((len (length text))
         (padding (max 0 (- total-width len))))
    (concatenate 'string text (make-string padding :initial-element pad-char))))

(defun center-offset (text-length total-width)
  "Calculate x offset to center text of TEXT-LENGTH in TOTAL-WIDTH."
  (max 0 (floor (- total-width text-length) 2)))

;;; ============================================================
;;; Side-effect functions - Screen drawing
;;; ============================================================

(defun draw-title-bar (scr title)
  "Draw a reversed-color title bar at the top of the screen."
  (let ((w (width scr)))
    (setf (attributes scr) '(:reverse))
    (move scr 0 0)
    (add-string scr (make-padded-string "" w))
    (move scr 0 (center-offset (length title) w))
    (add-string scr title)
    (setf (attributes scr) '())))

(defun draw-status-bar (scr message)
  "Draw a reversed-color status bar at the bottom of the screen."
  (let ((w (width scr))
        (h (height scr)))
    (setf (attributes scr) '(:reverse))
    (move scr (1- h) 0)
    (add-string scr (make-padded-string "" w))
    (move scr (1- h) 1)
    (add-string scr message)
    (setf (attributes scr) '())))

(defun draw-menu-items (scr items selected-index &key (start-y 3) (start-x 4))
  "Draw a list of menu items, highlighting the selected one."
  (loop for item in items
        for i from 0
        for y = (+ start-y i)
        do (move scr y start-x)
           (if (= i selected-index)
               (progn
                 (setf (attributes scr) '(:bold :reverse))
                 (add-string scr (format nil " > ~A " item))
                 (setf (attributes scr) '()))
               (add-string scr (format nil "   ~A  " item)))))

(defun draw-box-area (scr y x w h)
  "Draw a Unicode box at position (Y, X) with width W and height H."
  (let ((horizontal (make-string (- w 2) :initial-element #\─)))
    ;; Top border
    (move scr y x)
    (add-string scr (concatenate 'string "┌" horizontal "┐"))
    ;; Side borders
    (loop for row from 1 below (1- h)
          do (move scr (+ y row) x)
             (add-string scr "│")
             (move scr (+ y row) (+ x w -1))
             (add-string scr "│"))
    ;; Bottom border
    (move scr (+ y h -1) x)
    (add-string scr (concatenate 'string "└" horizontal "┘"))))

EOF

# Create src/main.lisp (use single-quote heredoc to prevent shell expansion,
# then sed to replace the project name placeholder)
cat > "$PROJECT_NAME/src/main.lisp" << 'MAINEOF'
(in-package :__PROJECT_NAME__)

;;; ============================================================
;;; Application state (pure data)
;;; ============================================================

(defstruct app-state
  "Holds the mutable state of the TUI application."
  (selected-index 0  :type fixnum)
  (running-p      t  :type boolean)
  (message        "" :type string))

(defparameter *menu-items*
  '("Hello World"
    "Show System Info"
    "About"
    "Quit")
  "List of menu items displayed in the TUI.")

(defparameter *app-title* "__PROJECT_NAME__ - TUI Application")

;;; ============================================================
;;; Menu action handlers (side-effect boundary)
;;; ============================================================

(defun handle-menu-action (state index)
  "Process the selected menu item and update state."
  (case index
    (0 (setf (app-state-message state)
             "Hello, World! Welcome to the TUI."))
    (1 (setf (app-state-message state)
             (format nil "SBCL ~A | ~A ~A"
                     (lisp-implementation-version)
                     (software-type)
                     (machine-type))))
    (2 (setf (app-state-message state)
             "Built with Common Lisp & croatoan."))
    (3 (setf (app-state-running-p state) nil)))
  state)

;;; ============================================================
;;; Render (side-effect: screen output)
;;; ============================================================

(defun render (scr state)
  "Render the full TUI screen from the current state."
  (clear scr)
  (draw-title-bar scr *app-title*)

  ;; Menu box
  (let* ((menu-y 2)
         (menu-x 2)
         (menu-w 30)
         (menu-h (+ (length *menu-items*) 2)))
    (draw-box-area scr menu-y menu-x menu-w menu-h)
    (draw-menu-items scr *menu-items*
                     (app-state-selected-index state)
                     :start-y (1+ menu-y)
                     :start-x (+ menu-x 2)))

  ;; Message area
  (let ((msg (app-state-message state)))
    (when (plusp (length msg))
      (move scr (+ 4 (length *menu-items*)) 4)
      (setf (attributes scr) '(:bold))
      (add-string scr msg)
      (setf (attributes scr) '())))

  ;; Status bar
  (draw-status-bar scr "[Up/k]Up [Down/j]Down [Enter]Select [q]Quit")
  (refresh scr))

;;; ============================================================
;;; Event handling (pure logic for key->state transitions)
;;; ============================================================

(defun move-selection-up (state)
  "Move selection up, wrapping around."
  (let ((idx (app-state-selected-index state))
        (len (length *menu-items*)))
    (setf (app-state-selected-index state)
          (mod (1- idx) len)))
  state)

(defun move-selection-down (state)
  "Move selection down, wrapping around."
  (let ((idx (app-state-selected-index state))
        (len (length *menu-items*)))
    (setf (app-state-selected-index state)
          (mod (1+ idx) len)))
  state)

;;; ============================================================
;;; Main entry point
;;; ============================================================

(defun main ()
  "Entry point for the TUI application."
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :cursor-visible nil
                    :enable-colors t)
    (let ((state (make-app-state)))
      (render scr state)
      (event-case (scr event)
        (#\q
         (return-from event-case))
        (:up
         (move-selection-up state)
         (render scr state))
        (:down
         (move-selection-down state)
         (render scr state))
        (#\k
         (move-selection-up state)
         (render scr state))
        (#\j
         (move-selection-down state)
         (render scr state))
        (#\Newline
         (handle-menu-action state (app-state-selected-index state))
         (if (app-state-running-p state)
             (render scr state)
             (return-from event-case)))
        (otherwise nil)))))

MAINEOF

# Replace placeholder with actual project name
sed -i "s/__PROJECT_NAME__/$PROJECT_NAME/g" "$PROJECT_NAME/src/main.lisp"

# Create run-build.sh
cat > "$PROJECT_NAME/run-build.sh" << EOF
#!/usr/bin/env sh

# Run build command
sbcl --eval "(require :asdf)" \\
     --eval "(push (uiop:getcwd) asdf:*central-registry*)" \\
     --load "quicklisp/setup.lisp" \\
     --eval "(ql:quickload :$PROJECT_NAME :silent t)" \\
     --eval "(asdf:make :$PROJECT_NAME)" \\
     --eval "(quit)"
EOF

chmod +x "$PROJECT_NAME/run-build.sh"

# Create run-tests.sh
cat > "$PROJECT_NAME/run-tests.sh" << EOF
#!/bin/bash

# Test runner script for $PROJECT_NAME
echo "Running tests for $PROJECT_NAME..."

sbcl --noinform \\
     --load "quicklisp/setup.lisp" \\
     --eval "(push (truename \".\") asdf:*central-registry*)" \\
     --eval "(push (truename \"./tests/\") asdf:*central-registry*)" \\
     --eval "(ql:quickload :$PROJECT_NAME-tests :silent t)" \\
     --eval "(asdf:test-system :$PROJECT_NAME-tests)" \\
     --eval "(quit)"
EOF

chmod +x "$PROJECT_NAME/run-tests.sh"

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

;;; ============================================================
;;; Pure function tests
;;; ============================================================

(defun test-make-padded-string ()
  "Test that make-padded-string pads correctly."
  (let ((result ($PROJECT_NAME::make-padded-string "Hi" 10)))
    (assert (= 10 (length result))
            () "Expected length 10, got ~A" (length result))
    (assert (string= "Hi" (subseq result 0 2))
            () "Expected 'Hi' prefix")))

(defun test-center-offset ()
  "Test center offset calculation."
  (assert (= 5 ($PROJECT_NAME::center-offset 10 20))
          () "Expected offset 5 for text=10, width=20")
  (assert (= 0 ($PROJECT_NAME::center-offset 30 20))
          () "Expected offset 0 when text wider than width"))

(defun test-move-selection ()
  "Test selection movement with wrapping."
  (let ((state ($PROJECT_NAME::make-app-state)))
    ;; Start at 0, move down
    ($PROJECT_NAME::move-selection-down state)
    (assert (= 1 ($PROJECT_NAME::app-state-selected-index state))
            () "Expected index 1 after move-down")
    ;; Move back up
    ($PROJECT_NAME::move-selection-up state)
    (assert (= 0 ($PROJECT_NAME::app-state-selected-index state))
            () "Expected index 0 after move-up")
    ;; Move up from 0 should wrap
    ($PROJECT_NAME::move-selection-up state)
    (assert (= (1- (length $PROJECT_NAME::*menu-items*))
               ($PROJECT_NAME::app-state-selected-index state))
            () "Expected wrap to last item")))

;;; ============================================================
;;; Test runner
;;; ============================================================

(defun run-tests ()
  (format t "~%Running tests...~%")
  (let ((tests '(test-make-padded-string
                 test-center-offset
                 test-move-selection))
        (passed 0)
        (failed 0))
    (dolist (test tests)
      (handler-case
          (progn
            (funcall test)
            (format t "  PASS ~A~%" test)
            (incf passed))
        (error (e)
          (format t "  FAIL ~A: ~A~%" test e)
          (incf failed))))
    (format t "~%Results: ~A passed, ~A failed~%" passed failed)
    (when (plusp failed)
      (sb-ext:exit :code 1))))

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
echo "TUI Project '$PROJECT_NAME' created successfully!"
echo ""
echo "Prerequisites:"
echo "  - ncurses 6.x development libraries must be installed"
echo "  - Ubuntu/Debian: sudo apt install libncursesw6 libncurses-dev"
echo ""
echo "Next steps:"
echo "  1. cd $PROJECT_NAME"
echo "  2. sbcl --script setup-quicklisp.lisp"
echo "  3. ./run-build.sh"
echo "  4. ./$PROJECT_NAME"
echo ""
echo "Happy hacking!"
