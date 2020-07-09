;;; core/core-features.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar rocket-emacs-enabled-features-list nil
  "Contains list of enabled features.")

(defmacro bind! (&rest args)
  "Acts as general-define-key."
  (declare (indent defun))
  `(general-define-key ,@args))

;; (defmacro package! pkg)
;; (declare (indent defun))
;; `(straight-use-package-

(cl-defun feature! (feature &key set disabled)
  "Add FEATURE to `rocket-emacs-enabled-features-list'."
  (declare (indent defun))
  (unless disabled
    (add-to-list 'rocket-emacs-enabled-features-list (symbol-name feature))))

(defun rocket-emacs-config-eval ()
  "Evaluate ~/.rocket-emacs/config.el."
  (condition-case nil
      (load rocket-emacs-personal-config nil 'nomessage)
    ((error "Error in config.el"))))

(defun rocket-emacs-features-init ()
  "Initialize Rocket Emacs features system."
  (rocket-emacs-config-eval)

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

  (straight-use-package 'use-package)

  ;; Set straight to use use-package
  (setq straight-use-package-by-default t)

  ;; General.el
  (use-package general
    :defer t
    :commands (general-define-key))

  ;; Iterate through files in the features directory
  (cl-dolist (x (directory-files-recursively rocket-emacs-features-dir "config"))
    ;; For example if the parent directory for a feature
    ;; is called auto-complete, it will try to execute
    ;; rocket-emacs-auto-complete-feature-init, but only
    ;; if auto-complete is in `rocket-emacs-enabled-features-list'
    (let ((feature-path (file-name-directory x)))
      (let ((y (split-string feature-path "\\/" t)))
	(let ((feature (nth (- (length y) 1) y)))
          (if (member feature rocket-emacs-enabled-features-list)
	      (progn
		(load (concat feature-path "packages.el") t 'nomessage)
		(load (concat feature-path "functions.el") t 'nomessage)
		(load (concat feature-path "config.el") t 'nomessage))))))))
