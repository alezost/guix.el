;;; guix-list.el --- Major mode for displaying Guix packages   -*- lexical-binding: t -*-

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

(require 'cl-lib)
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
    (define-key map (kbd "m")   'guix-list-mark)
    (define-key map (kbd "*")   'guix-list-mark)
    (define-key map (kbd "M")   'guix-list-mark-all)
    (define-key map (kbd "i")   'guix-list-mark-install)
    (define-key map (kbd "d")   'guix-list-mark-delete)
    (define-key map (kbd "u")   'guix-list-unmark)
    (define-key map (kbd "U")   'guix-list-unmark-all)
    (define-key map (kbd "DEL") 'guix-list-unmark-backward)
    map)
  "Keymap for `guix-list-mode'.")

(guix-define-buffer-type list tabulated-list-mode)

(defface guix-list-obsolete
  '((t :inherit guix-info-obsolete))
  "Face used if a package is obsolete."
  :group 'guix-list)

(defcustom guix-list-describe-package-warning-count 10
  "The maximum number of packages for describing without a warning.
If a user wants to \"\\[guix-list-describe-package]\" more than
this number of packages, he will be prompted for confirmation."
  :type 'integer
  :group 'guix-list)

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
  '((name        . guix-list-get-name)
    (synopsis    . guix-list-get-one-line)
    (description . guix-list-get-one-line)
    (installed   . guix-list-get-installed-outputs))
  "Alist for the values of package parameters inserted in columns.

Car of each assoc is a parameter name.

Cdr is a function returning a value that will be inserted.  The
function is called with 2 arguments: the first one is the value
of the parameter; the second argument is a package info (alist of
parameters and their values).")

(defvar guix-list-required-params '(id)
  "List of required package parameters.

Parameters displayed in a list buffer (columns) and parameters
from this list are received for every package.

May be a special value `all', in which case all supported
parameters are received (this may be very slow for lists with a
big number of packages).

Do not remove `id' from this list as it is required for
identifying a package.")

(defun guix-list-get-params-for-receiving ()
  "Return list of package parameters that should be received."
  (let ((params (mapcar #'car guix-list-column-format)))
    (mapc (lambda (param)
            (cl-pushnew param params))
          guix-list-required-params)
    params))

(defun guix-list-mode-initialize ()
  "Initial settings for `guix-list-mode'."
  (setq tabulated-list-padding 2)
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
            (list (guix-get-key-val 'id info)
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

(defun guix-list-get-name (name info)
  "Return NAME of the package.
Colorize it with `guix-list-obsolete' if needed."
  (guix-get-string name
                   (when (guix-get-key-val 'obsolete info)
                     'guix-list-obsolete)))

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

(defun guix-list-current-id ()
  "Return ID of the current package."
  (or (tabulated-list-get-id)
      (user-error "No package here")))

(defun guix-list-describe-package (&optional arg)
  "Describe packages marked with a general mark or current package.
With ARG (interactively with prefix), describe the packages
marked with any mark."
  (interactive "P")
  (let* ((ids (or (if arg
                      (guix-list-get-marked-id-list)
                    (guix-list-get-marked-id-list 'general))
                  (list (guix-list-current-id))))
         (count (length ids)))
    (when (or (<= count guix-list-describe-package-warning-count)
              (y-or-n-p (format "Do you really want to describe %d packages? "
                                count)))
      (guix-info-get-show-packages 'id ids))))

(defun guix-list-for-each-line (fun &rest args)
  "Call FUN with ARGS for each package line."
  (or (derived-mode-p 'guix-list-mode)
      (error "The current buffer is not in Guix List mode"))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply fun args)
      (forward-line))))

(defun guix-list-fold-lines (fun init)
  "Fold over package lines in the current list buffer.
Call FUN with RESULT as argument for each line, using INIT as
the initial value of RESULT.  Return the final result."
  (let ((res init))
    (guix-list-for-each-line
     (lambda () (setq res (funcall fun res))))
    res))


;;; Marking

(defvar guix-list-mark-alist
  '((empty   . ?\s)
    (general . ?*)
    (install . ?I)
    (delete  . ?D))
  "Alist of available mark names and mark characters.")

(defsubst guix-list-get-mark (name)
  "Return mark character by its NAME."
  (or (guix-get-key-val name guix-list-mark-alist)
      (error "Mark '%S' not found" name)))

(defsubst guix-list-get-mark-string (name)
  "Return mark string by its NAME."
  (string (guix-list-get-mark name)))

(defun guix-list-current-mark ()
  "Return mark character of the current package line."
  (char-after (line-beginning-position)))

(defun guix-list-get-marked-id-list (&rest mark-names)
  "Return list of IDs of the packages marked with any mark from MARK-NAMES.
If MARK-NAMES are not specified, use all marks from
`guix-list-mark-alist' except the `empty' one."
  (let ((marks (mapcar #'guix-list-get-mark
                       (or mark-names
                           (delq 'empty
                                 (mapcar #'car guix-list-mark-alist))))))
    (guix-list-fold-lines
     (lambda (ids)
       (if (memq (guix-list-current-mark) marks)
           (cons (guix-list-current-id) ids)
         ids))
     '())))

(defun guix-list-mark (name &optional advance)
  "Put a mark on the current package.
NAME is a mark name from `guix-list-mark-alist'.
If ADVANCE is non-nil, move forward by one line after marking.
Interactively, put a general mark and move to the next line."
  (interactive '(general t))
  (tabulated-list-put-tag (guix-list-get-mark-string name)
                          advance))

(defun guix-list-mark-all (name)
  "Mark all packages with NAME mark.
NAME is a mark name from `guix-list-mark-alist'.
Interactively, put a general mark on all packages."
  (interactive '(general))
  (guix-list-for-each-line #'guix-list-mark name))

(defun guix-list-mark-install ()
  "Mark the current package for installation and move to the next line."
  (interactive)
  (guix-list-mark 'install t))

(defun guix-list-mark-delete ()
  "Mark the current package for deletion and move to the next line."
  (interactive)
  (guix-list-mark 'delete t))

(defun guix-list-unmark ()
  "Unmark the package at point and move to the next line."
  (interactive)
  (guix-list-mark 'empty t))

(defun guix-list-unmark-backward ()
  "Move up one line and unmark the package there."
  (interactive)
  (forward-line -1)
  (guix-list-mark 'empty))

(defun guix-list-unmark-all ()
  "Unmark all packages."
  (interactive)
  (guix-list-mark-all 'empty))

(provide 'guix-list)

;;; guix-list.el ends here
