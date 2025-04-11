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
     --eval '(quicklisp-quickstart:install :path "~/quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
$ sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

### Set up SBCL in Emacs

Add below to `init.el` or `.emacs`.
If you are using Doom Emacs, add below to `config.el` file.

```lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
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

## Create and run a project with Quicklisp

### CASE 1) Create a project from scatch locally

Setup Quicklisp for your new project

```bash
$ cd
$ cd another-path

$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load ./quicklisp.lisp

CL-USER> (quicklisp-quickstart:install :path "./quicklisp")
CL-USER> (ql:add-to-init-file)                    ;; Optional
CL-USER> (ql:quickload :quicklisp-slime-helper)   ;; Optional
CL-USER> (quit)
```

Create new project `{project-name}` in `~/another-path/` and download `quicklisp.lisp` here.

```bash
$ sbcl

CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (ql:quickload :quickproject)
CL-USER> (quickproject:make-project #p"./" :name "{project-name}")
CL-USER> (quit)
```

Check your new project created.

```bash
$ tree
.
├── {project-name}
│   ├── {project-name}.asd
│   ├── {project-name}.lisp
│   ├── README.md
│   ├── quicklisp.lisp
│   └── quicklisp
│       └── setup.lisp
```

Load your project to SBCL REPL

```bash
$ sbcl

CL-USER> (load #p"./quicklisp/setup.lisp")
CL-USER> (push (truename ".") asdf:*central-registry*)
CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (quit)
```

### CASE 2) Create a project with `quickproject` 

```bash
$ sbcl

CL-USER> (ql:quickload :quickproject)
CL-USER> (quickproject:make-project #p"~/quicklisp/local-projects/{project-name}" :name "{project-name}")
CL-USER> (ql:quickload :{project-name})    ;; the project will be added to `system-index.txt`
CL-USER> (in-package :{project-name})
CL-USER> (quit)
```

Load the project again with Quicklisp in SBCL

```bash
$ sbcl

CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (quit)
```

Check `home` in REPL

```bash
$ sbcl

CL-USER> ql:*quicklisp-home*
CL-USER> (quit)
```

### CASE 3) Create a project in `~/quicklisp/local-projects/` manually

Create new project `{project-name}` in `~/quicklisp/local-projects/`

```bash
$ cd
$ cd quicklisp/local-projects
```

Let's name the project we are going to create `{project-name}`.

```bash
$ mkdir {project-name}
$ tree
.
├── {project-name}
│   ├── {project-name}.asd
│   ├── README.markdown
│   ├── README.org
│   ├── src
│   │   └── main.lisp
│   └── tests
│       └── main.lisp
```

Register local projects: `{project-name}`

```bash
$ sbcl

CL-USER> (ql:register-local-projects)
CL-USER> (quit)
```

the file `system-index.txt` will be created just like this

```bash
$ cat system-index.txt
{project-name}/{project-name}.asd
```

Load the project again with Quicklisp in SBCL

```bash
$ sbcl

CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (quit)
```

### NOTE: other ways to register local projects

Quicklisp의 local-projects 메커니즘 사용

```
ln -s $(pwd) ~/quicklisp/local-projects/
```

ASDF의 central-registry 사용

```
(push (truename ".") asdf:*central-registry*)
```

ASDF 소스 레지스트리 설정 파일 사용

```
(:directory (:home "path/to/your/project"))
```

Quicklisp의 local-project-directories 변수 사용

```
(push (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
```

### Load and run your project

```bash
$ sbcl

CL-USER> (ql:quickload :{project-name})
CL-USER> (in-package :{project-name})
CL-USER> (hello-world)
Hello, World!
```
### Build binary

```bash
./build.sh {project-name}
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
