;;; config.el -*- lexical-binding: t; -*-

(use-package counsel
  :after ivy
  :init
  (counsel-mode 1))

(use-package ivy
  :defer 2
  :init
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy)
