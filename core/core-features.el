;;; core-features.el --- Features system for Rocket Emacs  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'cl-lib)

(defvar rocket-emacs-enabled-features-list nil
  "Contains list of enabled features.")

(defmacro bind! (&rest args)
  "Acts as general-define-key."
  (declare (indent defun))
  `(general-define-key ,@args))

(cl-defun feature! (feature &key set disabled)
  "Add FEATURE to `rocket-emacs-enabled-features-list'."
  (declare (indent defun))
  (unless disabled
    (message set)
    (add-to-list 'rocket-emacs-enabled-features-list (symbol-name feature))))

(defun rocket-emacs-features-init ()
  "Initialize Rocket Emacs features system."

  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (require 'package)
  ;; Set package archvies and disable at startup
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-enable-at-startup nil)

  ;; Install dependency
  (straight-use-package 'use-package)

  ;; Set straight to use use-package
  (setq straight-use-package-by-default t)

  ;; General.el
  (use-package general
    :defer t
    :commands (general-define-key))

  ;; Iterate through files in the features directory
  (cl-dolist (feature-path (directory-files-recursively rocket-emacs-features-dir "config"))
    ;; For example if the parent directory for a feature
    ;; is called auto-complete, it will try to execute
    ;; rocket-emacs-auto-complete-feature-init, but only
    ;; if auto-complete is in `rocket-emacs-enabled-features-list'
    (let ((x (split-string feature-path "\\/" t)))
      (let ((y (nth (- (length x) 2) x)))
        (if (member y rocket-emacs-enabled-features-list)
	    ;; Load the feature's config.el and execute init function for that feature
	    (progn
	      (load feature-path nil 'nomessage)
	      (funcall (intern (concat "rocket-emacs-" y "-feature-init")))))))))

(provide 'rocket-emacs-core-features)
;;; core-features.el ends here
