# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands

**Prerequisites:** sbcl 2.6.0 (see `.tool-versions`) and Quicklisp. First-time setup:
```bash
sbcl --script setup-quicklisp.lisp
```

**Build standalone binary:**
```bash
./run-build.sh
# produces ./sbcl-cli-project executable
```

**Run tests:**
```bash
./run-tests.sh
```

**REPL with project loaded:**
```bash
sbcl --eval "(load \"quicklisp/setup.lisp\")" \
     --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
     --eval "(ql:quickload :sbcl-cli-project)"
```

## Architecture

This is a minimal SBCL CLI application using ASDF for system definition and Quicklisp for dependency management.

### System Structure

- `sbcl-cli-project.asd` — ASDF system definition with `program-op` build support and `:entry-point "sbcl-cli-project:main"`
- `src/main.lisp` — Defines the `sbcl-cli-project` package and `main` entry point function
- `tests/sbcl-cli-project-tests.asd` — Separate ASDF system for tests, depends on `fiveam`
- `tests/main-tests.lisp` — FiveAM test suite (`sbcl-cli-project-tests`) with `run!`-based execution

### Key Patterns

- **Package per file:** `src/main.lisp` defines and enters the `sbcl-cli-project` package; tests define `sbcl-cli-project-tests`
- **Build mechanism:** `run-build.sh` uses `asdf:make` which triggers `program-op` (SBCL's `save-lisp-and-die`) to produce a standalone binary
- **Test execution:** `run-tests.sh` registers both project root and `tests/` directory in `asdf:*central-registry*`, then runs via `asdf:test-system`
- **Test framework:** FiveAM with `def-suite`, `in-suite`, and `test` macros; test-op in `.asd` calls `fiveam:run!`
- **No external dependencies** for the main system; only `fiveam` for tests
