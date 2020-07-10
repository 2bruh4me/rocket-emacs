;;; features/base/config.el -*- lexical-binding: t; -*-

(bind!
  "C-x k" 'kill-this-buffer)
(bind!
  :prefix "C-."
  "e" '(:ignore t :which-key "emacs")
  "ei" '(:ignore t :which-key "input")
  "eii" 'set-input-method
  "eir" 'rocket/reset-input-method
  "f" '(:ignore t :which-key "file")
  "ff" 'find-file)

(defalias 'set-input-method 'rocket/set-input-method)

;; Disable default UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; Font

;; Use Y or N
(defalias 'yes-or-no-p 'y-or-n-p)

;; Optimize scrolling
(setq scroll-conservatively 101
      auto-window-vscroll nil
      scroll-margin 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t)

;; Disable default minibuffer message
(defun display-startup-echo-area-message ()
  (message nil))

;; Avoid GUI dialog
(setq use-dialog-box nil)

;; Inhibit splash screen
(setq inhibit-splash-screen t)

;; Set window (frame) title
(setq frame-title-format '("Rocket Emacs - %b")
      icon-title-format frame-title-format)

;; Set UTF-8
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Packages
(use-package recentf
  :straight nil
  :defer 1)

(use-package uniquify
  :straight nil
  :defer 2)

(use-package saveplace
  :straight nil
  :defer 2)

(use-package autorevert
  :straight nil
  :defer 2)

(use-package paren
  :straight nil
  :defer 2)

(use-package whitespace
  :straight nil
  :defer 2)

(use-package info+
  :straight nil
  :defer 2
  :load-path "features/rocket-emacs/base/lisp/info+")

(use-package dired+
  :straight nil
  :defer 2
  :load-path "features/rocket-emacs/base/lisp/dired+")

(use-package ace-link
  :defer 2
  :init
  (with-eval-after-load 'info
    (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'ace-link-help))
  (with-eval-after-load 'eww
    (define-key eww-link-keymap "o" 'ace-link-eww)
    (define-key eww-mode-map "o" 'ace-link-eww)))
