;;; config.el-*- lexical-binding: t; -*-

(define-derived-mode rocket-emacs/dashboard-mode special-mode "Dashboard")

(defun rocket-emacs-dashboard ()
  (switch-to-buffer (generate-new-buffer "Rocket Emacs Dashboard"))
  (rocket-emacs/dashboard-mode))

;; Add dashboard to startup hook
(add-hook 'emacs-startup-hook 'rocket-emacs-dashboard)
