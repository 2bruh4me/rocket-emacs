;;; core/core-features.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar rocket-emacs-enabled-features '()
  "Contains list of enabled features.")

(defmacro bind! (&rest args)
  "Acts as general-define-key."
  (declare (indent defun))
  `(general-define-key ,@args))

(cl-defun feature! (feature &key set disabled after)
  "Add FEATURE to `rocket-emacs-enabled-features'."
  (declare (indent defun))
  (unless disabled
    (set (intern (concat "rocket-emacs-" (symbol-name feature) "-feature-settings")) set)
    (add-to-list 'rocket-emacs-enabled-features (symbol-name feature))))

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

  ;; Set package archvies and disable at startup
  ;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-enable-at-startup nil)

  (straight-use-package 'use-package)

  ;; Set straight to use use-package
  (setq straight-use-package-by-default t)

  ;; General.el
  (use-package general
	       :defer t
	       :commands (general-define-key))

  (cl-dolist (feature rocket-emacs-enabled-features)
    (load (concat rocket-emacs-features-dir feature "/packages.el") t 'nomessage)
    (load (concat rocket-emacs-features-dir feature "/functions.el") t 'nomessage)
    (load (concat rocket-emacs-features-dir feature "/config.el") nil 'nomessage)))
