;;; init.el --- Entry point for Rocket Emacs         -*- lexical-binding: t; -*-

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

;; This file is the entry point for Rocket Emacs.
;; Here we make sure the Emacs version is correct and then
;; load the core.

;;; Code:

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

;; Start server if not already started
(require 'server)
(unless (server-running-p)
  (server-start))

;;; init.el ends here
