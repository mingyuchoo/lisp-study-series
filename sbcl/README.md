# README

## Install SBCL on macOS

Install SBCL with Brew

```bash
brew install sbcl
```

### Install QuickLISP and setup SBCL for Emacs

```bash
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

### Set up SBCL in Emacs

Add below to `init.el` file.
If you are using Doom Emacs, add below to `config.el` file.

```lisp
;; 1st.
;; Install `slime` package
;;
;; `M-x package-install RET slime RET`
;;

(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-contrib '(slime-fancy))

;; 2nd.
;; Start SLIME in Emacs
;;
;; `M-x slime`
;; 
```

## Starting SBCL

### From Shell to LISP

```bash
$ sbcl
* (+ 2 2)
4
* (exit)
$
```

### Running from Emacs

To run SBCL as a inferior-lisp from Emacs in your `.emacs` or `init.el`

```elisp
;; The SBCL binary and command-line arguments
(setq inferior-lisp-program "sbcl")
```

### Shebang Scripts

SBCL supports this via the `--script` command line option

Example file (`hello.lisp`):

```lisp
#! /opt/homebrew/bin/sbcl --script
(write-line "Hello, World!")
```

Usage examples:

```bash
$ ./hello.lisp
Hello, World!

$ sbcl --script hello.lisp
Hello, World!
```

### Compile SBCL on Windows 11

Write a compile script `compile-hello.lisp`

```lisp
(load "hello.lisp")
(sb-ext:save-lisp-and-die "hello.exe" :toplevel #'hello-world :executable t)
```

Run the script

```powershell
> sbcl --script compile-hello.lisp
> hello.exe
```
