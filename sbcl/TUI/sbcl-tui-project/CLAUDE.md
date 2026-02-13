# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands

```bash
# Initial setup (download Quicklisp + install croatoan dependency)
sbcl --script setup-quicklisp.lisp

# Build standalone binary
./run-build.sh

# Run tests
./run-tests.sh

# Run interactively in REPL
sbcl
(load #p"./quicklisp/setup.lisp")
(ql:quickload :sbcl-tui-project)
(sbcl-tui-project:main)
```

## Prerequisites

- SBCL 2.6.0 (managed via asdf `.tool-versions`)
- ncurses development libraries (`libncursesw6 libncurses-dev` on Debian/Ubuntu)

## Architecture

This is a Common Lisp TUI application using **croatoan** (ncurses bindings). The codebase follows a **pure/side-effect separation** pattern:

- **`src/package.lisp`** - Package definition. Exports only `:main`.
- **`src/screen-protocol.lisp`** - Generic function protocol wrapping croatoan screen operations (`scr-width`, `scr-height`, `scr-move`, `scr-add-string`, `scr-set-attrs`, `scr-clear`, `scr-refresh`). Default methods delegate to croatoan. Tests specialize these for mock objects.
- **`src/ui.lisp`** - UI layer split into:
  - Pure functions (`make-padded-string`, `center-offset`, `format-menu-item`, `make-box-top-border`, `make-box-bottom-border`) - no screen I/O, testable
  - Side-effect functions (`draw-title-bar`, `draw-status-bar`, `draw-menu-items`, `draw-box-area`) - use screen protocol
- **`src/main.lisp`** - Application core split into:
  - Pure data (`app-state` struct, `*menu-items*`)
  - Pure logic (`move-selection-up`, `move-selection-down`) - state transitions without I/O
  - Side-effect boundary (`handle-menu-action`, `render`, `main`) - event loop and screen rendering

Files load serially: `package.lisp` -> `screen-protocol.lisp` -> `ui.lisp` -> `main.lisp` (defined in `.asd`).

## Testing

Tests are in `tests/` with a separate ASDF system (`sbcl-tui-project-tests.asd`). Uses a custom test runner with `assert`-based tests (no external test framework).

- **Pure function tests**: `make-padded-string`, `center-offset`, `format-menu-item`, `make-box-top/bottom-border`, `move-selection-up/down`, `handle-menu-action` (all branches), `app-state` struct
- **Side-effect tests**: `draw-title-bar`, `draw-status-bar`, `draw-menu-items`, `draw-box-area`, `render` via `mock-screen` (implements screen protocol without terminal)
- **Untestable**: `main` (event loop requiring real terminal)

To add a test: define a `test-*` function in `tests/main-tests.lisp` and add it to the `tests` list in `run-tests`.

## Dependency Management

Quicklisp is installed **locally** in the project's `quicklisp/` directory (not system-wide). Both `run-build.sh` and `run-tests.sh` auto-setup Quicklisp if missing. The sole dependency is `croatoan`.

## Conventions

- Pure functions are grouped under `;;; Pure functions` section comments
- Side-effect functions are grouped under `;;; Side-effect functions` section comments
- Internal symbols are accessed with `::` in tests since only `:main` is exported
- Build scripts derive project name from directory name via `$(basename "$(pwd)")`
