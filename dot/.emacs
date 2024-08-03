;; ...

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; change the path to yours.
(setq inferior-lisp-program "~/.nix-profile/bin/sbcl")

(setq slime-contribs '(slime-fancy))



