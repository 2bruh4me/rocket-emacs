;;; config.el --- X window manager for Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  weebojensen

;; Author: weebojensen <weebojensen@wj01>
;; Keywords: 

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

(defun rocket-emacs-exwm-feature-init()
  (when (eq window-system 'x)
    (use-package exwm
      :defer nil
      :config
      (require 'exwm)
      (require 'exwm-config)
      (exwm-config-default))))

(provide 'rocket-emacs-exwm-feature)
;;; config.el ends here
