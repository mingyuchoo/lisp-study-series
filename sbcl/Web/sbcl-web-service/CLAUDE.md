# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands

**Prerequisites:** SBCL and Quicklisp must be installed. First-time setup:
```bash
sbcl --load setup-quicklisp.lisp --quit
```

**Start the server:**
```bash
./run-server.sh
# or directly:
sbcl --load initialize.lisp
```

**Run tests:**
```bash
./run-tests.sh
```

**Server configuration:** `config.env` (KEY=VALUE format, PORT and IP).

## Architecture

This is an SBCL/Hunchentoot web service using ASDF for system definition and Quicklisp for dependency management.

### Package/Module Hierarchy

```
sbcl-web-service          (main entry point, composes all modules)
├── sbcl-web-service.core  (server lifecycle + config loading)
├── sbcl-web-service.utils (logging, JSON helpers, timestamps)
├── sbcl-web-service.api   (JSON API route handlers)
└── sbcl-web-service.web   (HTML page route handlers)
```

### Request Flow

1. `initialize.lisp` bootstraps Quicklisp → loads ASDF system → calls `main`
2. `main` (src/main.lisp) loads dependencies → calls `initialize-routes` → calls `start-server`
3. `initialize-routes` clears `*dispatch-table*`, then registers web routes first, then API routes (order matters for prefix matching)
4. Hunchentoot dispatches requests via `*dispatch-table*` to handlers defined in `src/api/routes.lisp` and `src/web/routes.lisp`

### Key Patterns

- **Global state:** `*acceptor*` holds the running server instance; `*config*` holds plist-based configuration read from `config.env` at load time
- **Route registration:** Handlers are defined with `hunchentoot:define-easy-handler`, then registered into `*dispatch-table*` via `push` with prefix/regex dispatchers
- **Logging:** Custom level-based logging in utils (`:debug :info :warn :error :fatal`) outputs to stdout with ISO 8601 timestamps
- **JSON:** Uses `cl-json` for encoding/decoding; `json-response` sets content-type and status code
- **Tests:** FiveAM test suite in `tests/test-suite.lisp`; tests start/stop a real server on port 8088 and make HTTP requests via Drakma

### Dependencies

Defined in `sbcl-web-service.asd`: hunchentoot, cl-json, alexandria, cl-ppcre, cl-utilities, split-sequence. Test-only: fiveam, drakma.

## Conventions

- All source code is in `src/` organized by module, each module has its own `package.lisp`
- Static assets (HTML) are served from `static/`
- The `.asd` file is the source of truth for module load order (`:serial t`)
