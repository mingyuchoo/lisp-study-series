# README

## Install SBCL on macOS

Install SBCL with Brew

```bash
brew install sbcl
```

## Install QuickLISP and setup SBCL for Emacs

```bash
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
     --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
     --eval '(ql:add-to-init-file)' \
     --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
```

## Set up SBCL in Emacs

Add below to `init.el` file

```elisp
;; ...

(setq inferior-lisp-program "sbcl")
```
