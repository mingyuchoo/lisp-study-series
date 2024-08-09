# README

## Install CLISP on macOS

Install CLISP with Brew

```bash
$ brew install clisp
```


## How to run

### In a terminal

```bash
$ clisp
[1]>
[1]> (+ 3 (* 2 4))
11
[1]> (quit)
```

### In your Emacs

Add below to your config file (`.emacs` or `~/.emacs.d/init.el`)

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'slime)
  (package-refresh-contents)
  (package-install 'slime))
(setq inferior-lisp-program "clisp")
(require 'slime)
(slime-setup)
```

Run Emacs and then execute SLIME with `M-x slime`
