# sbcl-tui-project

A TUI (Terminal User Interface) application built with Common Lisp and croatoan.

## Prerequisites

- SBCL (Steel Bank Common Lisp)
- ncurses 6.x development libraries

```bash
# Ubuntu/Debian
sudo apt install libncursesw6 libncurses-dev

# Fedora/RHEL
sudo dnf install ncurses-devel

# macOS
brew install ncurses
```

## 1. Setup Quicklisp

```bash
cd sbcl-tui-project
sbcl --script setup-quicklisp.lisp
```

## 2. Load and run your project

```bash
sbcl

CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (ql:quickload :sbcl-tui-project)
CL-USER> (in-package :sbcl-tui-project)
sbcl-tui-project> (main)
```

## 3. Build binary

```bash
./run-build.sh
```

This will create an executable binary named `sbcl-tui-project`.

## 4. Run tests

```bash
./run-tests.sh
```

## Project Structure

```
sbcl-tui-project/
├── .gitignore
├── .tool-versions
├── README.md
├── run-build.sh
├── quicklisp.lisp
├── run-tests.sh
├── setup-quicklisp.lisp
├── sbcl-tui-project.asd
├── src/
│   ├── package.lisp
│   ├── ui.lisp
│   └── main.lisp
└── tests/
    ├── README.md
    ├── main-tests.lisp
    └── sbcl-tui-project-tests.asd
```

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
