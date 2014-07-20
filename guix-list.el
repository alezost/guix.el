;;; guix-list.el --- Major mode for displaying Guix packages

;; Copyright © 2014 Alex Kost

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

;; This file provides a list-like buffer for displaying information
;; about Guix packages.

;;; Code:

(require 'tabulated-list)
(require 'guix-info)
(require 'guix-history)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-utils)

(defvar guix-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'guix-list-describe-package)
    map)
  "Keymap for `guix-list-mode'.")

(guix-define-buffer-type list tabulated-list-mode)

(defvar guix-list-column-format
  '((name 20 t)
    (version 10 nil)
    (outputs 13 t)
    (installed 13 t)
    (synopsis 30 nil))
  "List of columns displayed in a buffer with a list of packages.
Each element of the list should have the form (NAME WIDTH SORT . PROPS).
PARAM is a name of the package parameter.
For the meaning of WIDTH, SORT and PROPS, see `tabulated-list-format'.")

(defvar guix-list-column-name-alist ()
  "Alist of titles of columns.
Each association is a cons of parameter name and column name.
If no parameter is not found in this alist, the value from
`guix-param-titles' is used for a column name.")

(defvar guix-list-column-value-alist
  '((synopsis    . guix-list-get-one-line)
    (description . guix-list-get-one-line)
    (installed   . guix-list-get-installed-outputs))
  "Alist for the values of package parameters inserted in columns.

Car of each assoc is a parameter name.

Cdr is a function returning a value that will be inserted.  The
function is called with 2 arguments: the first one is the value
of the parameter; the second argument is a package info (alist of
parameters and their values).")

(defvar guix-list-required-params '(name version)
  "List of required package parameters.

Parameters displayed in a list buffer (columns) and parameters
from this list are received for every package.

May be a special value `all', in which case all supported
parameters are received (this may be very slow for lists with a
big number of packages).

Do not remove `name' and `version' from this list as they are
required for identifying a package.")

(defun guix-list-get-params-for-receiving ()
  "Return list of package parameters that should be received."
  (let ((params (mapcar #'car guix-list-column-format)))
    (dolist (param guix-list-required-params params)
      (add-to-list 'params param))))

(defun guix-list-mode-initialize ()
  "Initial settings for `guix-list-mode'."
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-format (guix-list-get-list-format))
  (setq tabulated-list-sort-key
        (list (guix-get-param-title 'name)))
  (tabulated-list-init-header))

(defun guix-list-get-packages (search-type search-vals)
  "Search for Guix packages and return results.

See `guix-get-packages' for the meaning of SEARCH-TYPE and
SEARCH-VALS.

If `guix-list-required-params' is not `all', then parameters from
this list are appended to SEARCH-VALS."
  (guix-get-packages
   search-type
   (if (equal guix-list-required-params 'all)
       search-vals
     (append search-vals (guix-list-get-params-for-receiving)))))

(defun guix-list-insert-packages (packages)
  "Display PACKAGES in the current list buffer."
  (setq tabulated-list-entries (guix-list-get-entries packages))
  (tabulated-list-print))

(defun guix-list-get-list-format ()
  "Return package list specification for `tabulated-list-format'."
  (apply #'vector
         (mapcar
          (lambda (col-spec)
            (let ((name (car col-spec)))
              (cons (or (cdr (assq name guix-list-column-name-alist))
                        (guix-get-param-title name))
                    (cdr col-spec))))
          guix-list-column-format)))

(defun guix-list-get-entries (packages)
  "Return list of values for `tabulated-list-entries'.
Values are taken from PACKAGES which should have the form of
`guix-list-packages'."
  (mapcar (lambda (info)
            (list (guix-get-full-name info)
                  (guix-list-get-entry info)))
          packages))

(defun guix-list-get-entry (info)
  "Return array of values for `tabulated-list-entries'.
Package parameters are taken from INFO which should be an alist
of parameters and values."
  (apply #'vector
         (mapcar
          (lambda (col-spec)
            (let* ((param (car col-spec))
                   (val (guix-get-key-val param info))
                   (fun (cdr (assq param guix-list-column-value-alist))))
              (if (and val fun)
                  (funcall fun val info)
                (guix-get-string val))))
          guix-list-column-format)))

(defun guix-list-get-one-line (str _)
  "Return one-line string from a multi-line STR."
  (guix-get-one-line str))

(defun guix-list-get-installed-outputs (installed _)
  "Return string with outputs from INSTALLED list.
INSTALLED is a list of alists with additional parameters for
installed package."
  (guix-get-string
   (mapcar (lambda (info)
             (guix-get-key-val 'output info))
           installed)))

(defun guix-list-get-full-name ()
  "Return full name of the current package."
  (or (tabulated-list-get-id)
      (user-error "No package here")))

(defun guix-list-describe-package ()
  "Describe the current package."
  (interactive)
  (guix-info-get-show-packages 'name (guix-list-get-full-name)))

(provide 'guix-list)

;;; guix-list.el ends here
