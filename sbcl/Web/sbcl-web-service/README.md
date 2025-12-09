# SBCL Web Service

This project is a web service built using Steel Bank Common Lisp (SBCL). It provides a framework for creating and managing web applications with a focus on modularity and ease of use.

## Project Structure

- `src/`: Contains the source code for the web service.
  - `main.lisp`: Entry point of the web service, initializes the server and application logic.
  - `routes.lisp`: Defines routing logic and handles different HTTP routes.
  - `utils.lisp`: Contains utility functions for data validation and formatting.
  
- `tests/`: Contains the test suite for the web service.
  - `test-suite.lisp`: Includes test cases for the main application logic and routes.

- `.gitignore`: Specifies files and directories to be ignored by Git.

- `README.md`: Documentation for the project.

- `sbcl-web-service.asd`: ASDF system definition file for loading and compiling the project.

## Setup Instructions

1. Ensure you have SBCL installed on your machine.
2. Clone the repository:
   ```
   git clone <repository-url>
   ```
3. Navigate to the project directory:
   ```
   cd sbcl-web-service
   ```
4. Install Quicklisp (first time only):
   ```bash
   sbcl --load setup-quicklisp.lisp --quit
   ```
   This will install Quicklisp locally in the project directory and download all required dependencies.

5. Load and run the project using ASDF:
   ```lisp
   (push "/<ABSOLUTE-PATH>/sbcl-web-service/" asdf:*central-registry*)
   (asdf:load-system :sbcl-web-service)
   ```

## Usage Examples

### CASE 1: Starting the Web Service in SBCL REPL

To start the web service, run the following command in the SBCL REPL:

```bash
sbcl
```

```lisp
;; (Optional) If not already loaded by `~/.sbclrc`, load Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the dependencies
(ql:quickload "hunchentoot")

;; Load your application file
(load "src/main.lisp")

;; (Optional process) If the server doesn't start automatically, you can start it manually:
(in-package :sbcl-web-service)
(start-server 8080) ;; use a different port if 8080 is already in use

;; Stop the web server
(stop-server)
```

### CASE 2: Starting the Web Service in a Shell

To start the web service, run the following command in the SHELL:

```bash
./run-server.sh
# or
sbcl --load initialize.lisp
```

You can then access the web service at `http://localhost:8080`.

## Running Tests in SBCL REPL

To run the tests, run the following command in the SBCL REPL:

```lisp
;; (Optional) If not already loaded by `~/.sbclrc`, load Quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the test dependencies
(ql:quickload '(:fiveam :drakma))

;; Load the test system
(ql:quickload :sbcl-web-service/tests)

;; Run the tests
(sbcl-web-service.tests:run-tests)

;; For more detailed output
(sbcl-web-service.tests:run-tests-interactive)
```

## Contributing

Feel free to submit issues or pull requests to improve the project. Please ensure that your contributions adhere to the project's coding standards and include appropriate tests.