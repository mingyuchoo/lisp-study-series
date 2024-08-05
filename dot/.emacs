;; ...

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; The SBCL binary and command-line arguments
(after! slime
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "rlwrap sbcl"))

;; change the path to yours.
(setq inferior-lisp-program "~/.nix-profile/bin/sbcl")
