;;; guix.el --- Interface for GNU Guix package manager

;; Copyright © 2014 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Version: 0.01
;; Package-Requires: ((geiser "0.3"))
;; URL: https://github.com/alezost/guix.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is under development.  The goal is to make a
;; full-featured Emacs interface for Guix package manager
;; <http://www.gnu.org/software/guix/>.

;; Currently the package provides an interface for searching, listing
;; and getting information about Guix packages.

;; To install this package, add the following to your init-file:
;;
;;   (add-to-list 'load-path "/path/to/guix-dir")
;;   (autoload 'guix-search-by-name "guix" nil t)
;;   (autoload 'guix-search-by-regexp "guix" nil t)
;;   (autoload 'guix-all-available-packages "guix" nil t)
;;   (autoload 'guix-newest-available-packages "guix" nil t)

;;; Code:

(require 'guix-list)

(defgroup guix nil
  "Interface for Guix package manager."
  :prefix "guix-"
  :group 'external)

(defvar guix-search-params '(name synopsis description)
  "Default list of parameters for searching by regexp.
Parameters are symbols from `guix-param-alist'.")

(defvar guix-search-history nil
  "A history of minibuffer prompts.")

;;;###autoload
(defun guix-search-by-name (name)
  "Search for Guix packages by NAME.
NAME is a string with name specification.  It may optionally contain
a version number.  Examples: \"guile\", \"guile-2.0.11\"."
  (interactive
   (list (read-string "Package name: " nil 'guix-search-history)))
  (guix-list-get-show-packages 'name name))

;;;###autoload
(defun guix-search-by-regexp (regexp &rest params)
  "Search for Guix packages by REGEXP.
PARAMS are package parameters that should be searched.
If PARAMS are not specified, use `guix-search-params'."
  (interactive
   (list (read-string "Regexp: " nil 'guix-search-history)))
  (or params (setq params guix-search-params))
  (guix-list-get-show-packages 'regexp regexp params))

;; ;;;###autoload
;; (defun guix-installed-packages ()
;;   "Display information about installed Guix packages."
;;   (interactive)
;;   (guix-list-get-show-packages 'installed))

;;;###autoload
(defun guix-all-available-packages ()
  "Display information about all available Guix packages."
  (interactive)
  (guix-list-get-show-packages 'all-available))

;;;###autoload
(defun guix-newest-available-packages ()
  "Display information about the newest available Guix packages."
  (interactive)
  (guix-list-get-show-packages 'newest-available))

(provide 'guix)

;;; guix.el ends here
