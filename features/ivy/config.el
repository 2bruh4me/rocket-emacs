;;; config.el --- Ivy feature                        -*- lexical-binding: t; -*-

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

(defun rocket-emacs-ivy-feature-init ()
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
    :after ivy))
(provide 'rocket-emacs-ivy-feature)
;;; config.el ends here
