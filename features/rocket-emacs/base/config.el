;;; base.el --- Base feature for Rocket Emacs        -*- lexical-binding: t; -*-

;; Author: 2bruh4me
;; URL: https://github.com/2bruh4me/rocket-emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Yep

;;; Code:

(defun rocket-emacs-base-feature-init ()
  "Initialize base feature."

  ;; Deactivate input method function
  (defun rocket/reset-input-method ()
    (interactive)
    (if current-input-method
	(deactivate-input-method)))

  ;; Set input method function
  ;; TODO don't make this just a copy of set-input-method
  (defalias 'set-input-method 'rocket/set-input-method)
  (defun rocket/set-input-method (input-method &optional interactive)
    (interactive
     (let* ((default (or (car input-method-history) default-input-method)))
       (list (read-input-method-name
              (if default "Select input method (default %s): " "Select input method: ")
              default t)
             t)))
    (activate-input-method input-method)
    (setq default-input-method input-method)
    (when interactive
      (customize-mark-as-set 'default-input-method))
    default-input-method)

  ;; Keybinds
  (defun rocket-emacs-base-feature-keybinds-init ()
    (bind!
     "C-x k" 'kill-this-buffer)
    (bind!
     :prefix "C-."
     "e" '(:ignore t :which-key "emacs")
     "ei" '(:ignore t :which-key "input")
     "eii" 'set-input-method
     "eir" 'rocket/reset-input-method
     "f" '(:ignore t :which-key "file")
     "ff" 'find-file))

  ;; Disable default UI
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1)

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

  ;; Initialize keybinds
  (rocket-emacs-base-feature-keybinds-init)

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
      (define-key eww-mode-map "o" 'ace-link-eww))))

(provide 'rocket-emacs-base-feature)
;;; base.el ends here
