# Common Lisp Web Application

A full-featured web application written in Common Lisp using the Hunchentoot web server and ASDF system definition. This project demonstrates how to build and structure a complete web application in Common Lisp with proper separation of concerns.

## Features

- Built with Hunchentoot, the de facto standard web server for Common Lisp
- Complete ASDF system structure with proper dependency management
- HTML generation using CL-WHO
- CSS generation using CL-CSS
- JavaScript generation using Parenscript
- Multiple page templates and static assets
- JSON API endpoints
- Dynamic content generation

## Technical Stack

- **Web Server:** Hunchentoot
- **HTML Generation:** CL-WHO
- **JavaScript Generation:** Parenscript
- **CSS Generation:** CL-CSS
- **Package Management:** Quicklisp and ASDF

## Project Structure

```
.
├── app.lisp              # Main application code
├── cl-webapp.asd         # ASDF system definition
├── package.lisp          # Package definitions
├── simple-server.lisp    # Server starter script
├── static/               # Static assets directory
│   ├── css/              # CSS files
│   ├── js/               # JavaScript files
├── templates.lisp        # HTML templates
├── quicklisp.lisp        # Quicklisp
└── setup-quicklisp.lisp  # Script for setting up quicklisp.lisp
```

## Running the Application

### Prerequisites

- Steel Bank Common Lisp (SBCL)
- Quicklisp

### Setup and Running

1. Clone the repository
2. Make sure Quicklisp is installed and set up
3. Run the application:

```bash
sbcl --script setup-quicklisp.lisp
sbcl --script simple-server.lisp
```

The server will start on port 5000. Open your browser and navigate to http://localhost:5000

## API Endpoints

The application provides several RESTful JSON endpoints:

- `GET /api/info` - Returns information about the application
- `GET /api/status` - Returns the current server status

Example:
```bash
curl -X GET http://localhost:5000/api/info
```

## Pages

The application includes the following pages:

- `/` - Home page with an overview of the application
- `/about` - Information about the project
- `/features` - Detailed feature list
- `/api` - API documentation

## License

MIT
