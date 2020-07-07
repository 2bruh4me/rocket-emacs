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

;; Keybinds
(defun rocket-emacs-base-feature-keybinds-init ()
  (bind!
   :prefix "C-."
   "f" 'find-file))

(defun rocket-emacs-base-feature-init ()

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

  ;; Set scratch as initial buffer
  (setq inhibit-splash-screen t)

  ;; Avoid GUI dialog
  (setq use-dialog-box nil)

  ;; Set window (frame) title
  (setq frame-title-format '("Rocketmacs - %b")
        icon-title-format frame-title-format)

  ;; Set UTF-8
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; Dashboard
  (defun rocket-emacs-dashboard ())

  ;; Add dashboard to startup hook
  (add-hook 'emacs-startup-hook 'rocket-emacs-dashboard)

  ;; Initialize keybinds
  (rocket-emacs-base-feature-keybinds-init))

(provide 'rocket-emacs-base-feature)
;;; base.el ends here
