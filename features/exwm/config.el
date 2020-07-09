;;; config.el -*- lexical-binding: t; -*-

(when (eq window-system 'x)
  (use-package exwm
    :defer nil
    :config
    (require 'exwm)
    (require 'exwm-config)
    (exwm-config-default)))
