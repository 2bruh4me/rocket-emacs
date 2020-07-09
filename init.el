;;; init.el -*- lexical-binding: t; -*-

;; Set garbage collection threshold to maximum possible value so
;; Emacs doesn't do any garbage collections when we load the core
(setq gc-cons-threshold most-positive-fixnum)

;; Check Emacs version
(when (version< emacs-version "26.3")
  (error "Please update your Emacs to 26.3 or newer"))

;; Set user-emacs-directory to the directory this file is
;; executing from
(setq user-emacs-directory (file-name-directory load-file-name))

;; Load the core
(load (concat user-emacs-directory "core/core") nil 'nomessage)

;; Initialize
(rocket-emacs-init)
