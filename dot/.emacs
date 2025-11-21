;;; .emacs --- Personal Emacs Configuration
;;; Commentary:
;; Optimized and modular Emacs configuration
;; Author: mgch
;; Created: 2025

;;; Code:

;; Suppress deprecated cl package warnings
(setq byte-compile-warnings '(not cl-functions obsolete))
(setq warning-suppress-log-types '((package reinitialization) (cl)))
(setq warning-suppress-types '((package reinitialization) (cl)))

;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

;; Initialize package system
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Install use-package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ============================================================================
;; SYSTEM CONFIGURATION
;; ============================================================================

;; Platform-specific settings
(defvar choo/is-windows (string-equal system-type "windows-nt"))
(defvar choo/is-unix (not choo/is-windows))

;; Set home directory and paths
(defvar choo/home-directory
  (if choo/is-windows
      "c:/Users/mingy"
    (getenv "HOME")))


;; Unix-specific path setup
(when choo/is-unix
  (add-to-list 'exec-path (concat ".cargo/bin" "/.local/bin/"))
  (add-to-list 'load-path (concat "/.opam/default/share/emacs/site-lisp")))

;; ============================================================================
;; BASIC SETTINGS
;; ============================================================================

;; Essential libraries
(require 'cl-lib)
(require 'whitespace)

;; UI Settings
(setq-default
      inhibit-startup-message t
      initial-scratch-message ""
      display-line-numbers t
      column-number-mode t
      indent-tabs-mode nil
      tab-width 2
      tab-always-indent 'complete
      require-final-newline t
      make-backup-files nil
      ring-bell-function 'ignore
      show-paren-style 'parenthesis
      scroll-preserve-screen-position 'always
      resize-mini-windows nil
      message-log-max nil
      font-lock-maximum-decoration t)

;; Whitespace configuration
(setq whitespace-style '(face tabs tab-mark))
(set-face-attribute 'whitespace-tab nil
                    :foreground "grey"
                    :background nil
                    :weight 'thin)

;; Enable useful modes
(global-prettify-symbols-mode t)
(global-whitespace-mode t)

;; Disable unwanted UI elements
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Enable useful modes
(display-time-mode t)
(display-battery-mode t)

;; Theme
(load-theme 'wheatgrass t)

;; Set both default and initial frames
(setq default-frame-alist '((width . 120) (height .40)))
(setq initial-frame-alist '((width . 120) (height .40)))

;; Font configuration
(set-face-attribute 'default nil
                    :family "D2CodingLigature Nerd Font"
                    :foundry "DAMA"
                    :slant 'normal
                    :weight 'regular
                    :height 160
                    :width 'normal)

;; Cursor
(set-face-attribute 'cursor nil :background "red3")

;; Clean up messages buffer
(when (get-buffer "*Messages*")
  (kill-buffer "*Messages*"))
;; ============================================================================
;; UTILITY PACKAGES
;; ============================================================================

;; Transpose frame
(use-package transpose-frame
  :bind ("C-x t" . transpose-frame))

;; Dotenv mode
(use-package dotenv-mode)

;; EditorConfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-S-a" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Helm
(use-package helm
  :config
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)))

;; VTerm
(use-package vterm
  :bind ("C-x v" . vterm-toggle))

;; Neotree
(use-package neotree
  :config
  (setq neo-smart-open t
        neo-window-width 30)
  :bind ("C-x n" . neotree-toggle))
;; ============================================================================
;; ORG MODE CONFIGURATION (Unix only)
;; ============================================================================

(when choo/is-unix
  (defvar choo/org-roam-directory (concat choo/home-directory "/Documents/org-roam"))

  ;; Org bullets
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; Org Babel languages
  (use-package ob-rust)
  (use-package ob-typescript)

  ;; Org Roam
  (use-package org-roam
    :config
    (setq org-roam-directory (file-truename choo/org-roam-directory)
          org-agenda-files (list choo/org-roam-directory))
    (org-roam-db-autosync-mode)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)))

  ;; Org mode configuration
  (with-eval-after-load 'org
    (setq org-startup-folded 'content
          org-startup-indented t
          org-hide-emphasis-markers t
          org-log-done 'time
          org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "DONE"))
          org-confirm-babel-evaluate nil)

    ;; Babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (js . t)
       (rust . t)
       (haskell . t)
       (ocaml . t)))

    ;; Org mode hooks
    (add-hook 'org-mode-hook
              (lambda ()
                (display-line-numbers-mode 0)
                (visual-line-mode t)
                (local-set-key (kbd "C-c a") 'org-agenda)))))

;; ============================================================================
;; PROGRAMMING LANGUAGES
;; ============================================================================

;; Common Lisp (SBCL + SLIME)
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))
(setq inferior-lisp-program "rlwrap sbcl")

;; Haskell
(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-snippet t
        lsp-completion-enable t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t))

(use-package lsp-haskell
  :after (haskell-mode lsp-mode)
  :config
  (setq lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper")
  :hook
  ((haskell-mode . lsp)
   (lsp-mode . (lambda ()
                 (add-hook 'before-save-hook 'lsp-format-buffer nil t)))))

;; OCaml
(use-package dune)
(use-package ocamlformat)
(use-package ocp-indent)
(use-package opam)

;; Zig
(use-package zig-mode)

;; Rust
(use-package rust-mode)

;; Erlang & Elixir
(use-package erlang)
(use-package alchemist)

(use-package elixir-mode
  :config
  (setq lsp-elixir-enable-inlay-hints t)
  :hook ((elixir-mode . eglot-ensure)
         (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "~/.nix-profile/bin/elixir-ls")))

;; Nix
(use-package nix-mode)
;; ============================================================================
;; HOOKS AND MINOR CONFIGURATIONS
;; ============================================================================

;; Disable line numbers for specific modes
(dolist (mode '(term-mode-hook eshell-mode-hook dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Setup Eshell prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (format-time-string "%H:%M" (current-time))
                     'face '(:foreground "cyan"))
         " "
         (propertize (abbreviate-file-name (eshell/pwd))
                     'face '(:foreground "yellow" :weight bold))
         "\n"
         (propertize "λ " 'face '(:foreground "green"))
         )))

(setq eshell-prompt-regexp "^λ ")


;; Cleanup before saving
(add-hook 'before-save-hook
          (lambda ()
            (whitespace-cleanup)
            (delete-trailing-whitespace)))

;; Helper function for window setup (commented out by default)
(defun setup-initial-windows ()
  "Split the window and open eshell in the bottom window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (eshell)
  (other-window 1))
;; (add-hook 'after-init-hook 'setup-initial-windows)
;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(defun split-window-below-and-move ()
  "Split the window below and move the cursor to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-and-move ()
  "Split the window right and move the cursor to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun search-and-replace-current-word ()
  "Search for the current word and initiate a query-replace."
  (interactive)
  (let ((current-word (thing-at-point 'word)))
    (if current-word
        (progn
          (isearch-yank-string current-word)
          (isearch-exit)
          (query-replace current-word (read-string (format "Replace %s with: " current-word))))
      (message "No word at point."))))

(defun replace-current-word-everywhere ()
  "Replace all instances of the current word in the buffer."
  (interactive)
  (let ((current-word (thing-at-point 'word)))
    (if current-word
        (let ((replacement (read-string (format "Replace %s with: " current-word))))
          (save-excursion
            (goto-char (point-min))
            (replace-string current-word replacement)))
      (message "No word at point."))))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Navigation with recentering
(global-set-key [next] (lambda () (interactive) (scroll-up-command) (recenter)))
(global-set-key [prior] (lambda () (interactive) (scroll-down-command) (recenter)))
(global-set-key (kbd "C-n") (lambda () (interactive) (next-line) (recenter)))
(global-set-key (kbd "C-p") (lambda () (interactive) (previous-line) (recenter)))

;; Tab management
(global-set-key (kbd "M-=") 'tab-bar-new-to)
(global-set-key (kbd "M--") 'tab-bar-close-tab)

;; Custom functions
(global-set-key (kbd "C-c r") 'search-and-replace-current-word)
(global-set-key (kbd "C-c a") 'replace-current-word-everywhere)

;; Window management
(global-set-key (kbd "C-x 2") 'split-window-below-and-move)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move)

;; Backspace behavior
(global-set-key (kbd "C-h") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "C-h") 'delete-backward-char)

;; Smartparens (if available)
(when (fboundp 'sp-backward-unwrap-sexp)
  (global-set-key (kbd "M-[ I") 'sp-backward-unwrap-sexp)
  (global-set-key (kbd "M-[ O") 'sp-backward-unwrap-sexp))

;; ============================================================================
;; CUSTOM VARIABLES (Auto-generated)
;; ============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(leuven-dark))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(package-selected-packages nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UbuntuMono Nerd Font" :foundry "DAMA" :slant normal :weight regular :height 120 :width normal)))))

;;; .emacs ends here
