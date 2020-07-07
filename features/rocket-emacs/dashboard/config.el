;;; config.el --- Dashboard for Rocket Emacs         -*- lexical-binding: t; -*-

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

(defun rocket-emacs-dashboard-feature-init ()
  "Initialize dashboard feature."
  (defun rocket-emacs-dashboard ()
    ())

  ;; Add dashboard to startup hook
  (add-hook 'emacs-startup-hook 'rocket-emacs-dashboard))

(provide 'config)
;;; config.el ends here
