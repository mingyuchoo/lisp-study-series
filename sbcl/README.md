# README

## Install SBCL

### Install with ASDF

```bash
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.15.0
. "$HOME/.asdf/asdf.sh"
. "$HOME/.asdf/completions/asdf.bash"
sudo apt install -y libzstd-dev libssl-dev automake autoconf libncurses5-dev
sudo apt install -y dirmngr gpg curl gawk jq rlwrap
asdf plugin add sbcl https://github.com/smashedtoatoms/asdf-sbcl.git
asdf install sbcl latest
asdf global sbcl latest
```
`vim $HOME/.tool-versions`

```bash
# $HOME/.tool-versions

sbcl 2.5.0
```

### Install SBCL on macOS

Install SBCL with Brew

```bash
$ brew install sbcl rlwrap
```

## Install QuickLISP and setup SBCL for Emacs

### Install QuickLISP

- SBCL: an implementation of the Common Lisp programming language
- CLISP: an implementation of the Common Lisp programming language
- Quicklisp: a library manager for Common Lisp
- asdf: a build tool for Common Lisp; come with asdf included already.

```bash
$ curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
$ sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
$ sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

### Set up SBCL in Emacs

Add below to `init.el` or `.emacs`.
If you are using Doom Emacs, add below to `config.el` file.

```lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "rlwrap sbcl")
```

## Starting SBCL

### From Shell to LISP

```bash
$ sbcl
CL-USER> (+ 2 2)
4
CL-USER> (exit)
$
```

### Running from Emacs

You must start SLIME in Emacs first.

```lisp
M-x slime
```

After reading a list file into Emacs,
evaluate or apply it with the following keybindings.

- Evaluate a function definition
`C-c C-c`

- Call a function in the buffer `*slime-repl sbcl*`
`C-C C-z`

- Call a function in the REPL
`CL-USER> (function-name args..)`

- Apply a function in a source file
`C-x C-e`

- Evaludate a region selected in a buffer (after selecting a region)
`C-c C-r`

- Evaludate the entire buffer
`C-c C-k`

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

## Compile a lisp file using SBCL

Write a compile script `compile-hello.lisp`

```lisp
(load "hello.lisp")
(sb-ext:save-lisp-and-die "hello.exe" :toplevel #'hello-world :executable t)
```

Run the script

```bash
> sbcl --script compile-hello.lisp
> hello.exe
```

## Create a project with ASDF

Let's name the project we are going to create `demo-cl`.

```bash
$ sbcl
```

```lisp
CL-USER> (ql:quickload "quickproject")
CL-USER> (quickproject:make-project #p"~/.quicklisp/local-projects/{project-name}" :name "{project-name}")
CL-USER> (ql:quickload "{project-name}")
CL-USER> (in-package "{project-name}")
```

## How to find and check the function signatures

### In SBCL

describe 함수를 사용하여 특정 함수나 심볼에 대한 정보 확인

```lisp
(describe '<symbol-name>)
```

documentation 함수를 사용하여 특정 함수나 심볼에 대한 정보 확인

```lisp
(documentation '<symbol-name> 'function)
```

### In SLIME on Emacs

특정 함수나 심볼 이름 위에 커서를 두고 
`C-c C-d h` 또는 `M-x slime-describe-symbol`를 
입력해서 확인
