;;; core/core.el -*- lexical-binding: t; -*-

;; CL-lib for Common Lisp features
(require 'cl-lib)

(defconst rocket-emacs-version "0.0.3"
  "Version number for Rocket Emacs.")

(defconst rocket-emacs-init-p nil
  "Non-nil if Rocket Emacs has initialized.")

;; Paths
(defconst rocket-emacs-dir user-emacs-directory
  "The directory where init.el lies.")

(defconst rocket-emacs-core-dir (concat rocket-emacs-dir "core/")
  "The directory where the core files for Rocket Emacs lies.")

(defconst rocket-emacs-features-dir (concat rocket-emacs-dir "features/"))

(defconst rocket-emacs-assets-dir (concat rocket-emacs-dir "assets/")
  "The directory where the assets for Rocket Emacs lies.")

(defconst rocket-emacs-templates-dir (concat rocket-emacs-assets-dir "templates/"))

(defconst rocket-emacs-documentation-dir (concat rocket-emacs-dir "docs/")
  "The directory where the documentation for Rocket Emacs lies.")

(defconst rocket-emacs-cache-dir (concat rocket-emacs-dir ".cache/")
  "The directory where cache files ares stored.")

(defconst rocket-emacs-home-dir (expand-file-name "~/")
  "The user's home directory.")

(defconst rocket-emacs-personal-dir (concat rocket-emacs-home-dir ".rocket-emacs/")
  "The user's personal files lies in this directory.")

(defconst rocket-emacs-personal-config (concat rocket-emacs-personal-dir "config.el"))

(defconst rocket-emacs-personal-themes-dir (concat rocket-emacs-personal-dir "themes/"))

(defconst rocket-emacs-personal-snippets-dir (concat rocket-emacs-personal-dir "snippets/"))

;; User configuration variables
(defvar rocket-emacs-config-gc-threshold 800000)

;; Other
(defvar rocket-emacs-session-type nil)
(defconst rocket-emacs-system-type system-type)
(defvar rocket-emacs-first-run-p nil)

;; Function for creating .rocket-emacs directory
(defun rocket-emacs-create-personal-dir ()
  "Create the .rocket-emacs directory."
  (make-directory rocket-emacs-personal-dir)
  (make-directory rocket-emacs-personal-snippets-dir)
  (make-directory rocket-emacs-personal-themes-dir)
  (copy-file (concat rocket-emacs-templates-dir "config.el") rocket-emacs-personal-config)
  (copy-file (concat rocket-emacs-templates-dir "personal-local.el")  (concat rocket-emacs-personal-dir ".dir-locals.el")))

;; Function for initializing Rocket Emacs
(cl-defun rocket-emacs-init ()
  "Initialize Rocket Emacs."

  ;; Check if Rocket Emacs already has been initialized
  (if rocket-emacs-init-p
      (cl-return-from rocket-emacs-init)

    ;; Check if first run
    (unless (file-directory-p rocket-emacs-personal-dir)
      (setq rocket-emacs-first-run-p t))

    ;; Check for git executable
    (unless (locate-file "git" exec-path)
      (error "Please install git for Rocket Emacs to work"))

    ;; If first run create all necessary files and restart
    (if rocket-emacs-first-run-p
        (progn
          (rocket-emacs-create-personal-dir)))

    (load (concat rocket-emacs-core-dir "core-features") nil 'nomessage)
    (rocket-emacs-features-init)

    ;; Set garbage collection threshold and clear echo area
    (add-hook 'after-init-hook
              (lambda ()
                (setq gc-cons-threshold rocket-emacs-config-gc-threshold)))))
