;;; guix-info.el --- Major mode for displaying Guix packages

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

;; This file provides a help-like buffer for displaying information
;; about Guix packages.

;;; Code:

(require 'guix-history)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-utils)

(defvar guix-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-buffer-map
                               special-mode-map))
    map)
  "Keymap for `guix-info-mode'.")

(guix-define-buffer-type info special-mode)

(defface guix-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for a title of a package parameter."
  :group 'guix-info)

(defface guix-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-info)

(defface guix-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-info)

(defface guix-info-synopsis
  '((t :inherit font-lock-doc-face))
  "Face used for a synopsis of a package."
  :group 'guix-info)

(defface guix-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-info)

(defface guix-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-info)

(defface guix-info-location
  '((t :inherit button))
  "Face used for a location of a package."
  :group 'guix-info)

(defface guix-info-url
  '((t :inherit button))
  "Face used for URLs."
  :group 'guix-info)

(defface guix-info-inputs
  '((t :inherit button))
  "Face used for inputs of a package."
  :group 'guix-info)

(defface guix-info-native-inputs
  '((t :inherit guix-info-inputs))
  "Face used for native inputs of a package."
  :group 'guix-info)

(defcustom guix-info-ignore-empty-vals nil
  "If non-nil, do not display parameters with nil values."
  :type 'boolean
  :group 'guix-info)

(defvar guix-info-param-title-format "%-15s: "
  "String used to format a title of each package parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.")

(defvar guix-info-multiline-prefix (make-string 17 ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.")

(defvar guix-info-fill-column 60
  "Column used for filling (word wrapping) parameters with long lines.
If a value is not multi-line and it occupies more than this
number of characters, it will be split into several lines.")

(defvar guix-info-delimiter "\n\f\n"
  "String used to separate packages info.")

(defvar guix-info-insert-param-alist
  '((name          . guix-info-name)
    (version       . guix-info-version)
    (license       . guix-info-license)
    (synopsis      . guix-info-synopsis)
    (description   . guix-info-description)
    (outputs       . guix-info-outputs)
    (home-url      . guix-info-insert-url)
    (inputs        . guix-info-insert-inputs)
    (native-inputs . guix-info-insert-native-inputs)
    (location      . guix-info-insert-location))
  "Methods of inserting package parameters.
Car of each assoc is a symbol from `guix-param-alist'.
Cdr is either a face name, used for the inserted value, or a
function, which is called with the value as the argument.")

(defvar guix-info-params
  '(name version outputs synopsis location home-url license
    inputs native-inputs description)
  "List of parameters displayed in the info buffer.
Each parameter should be a symbol from `guix-param-alist'.
The order of displayed parameters is the same as in this list.
If nil, display all parameters with no particular order.")

(defalias 'guix-info-get-packages 'guix-get-packages)

(defun guix-info-insert-packages (packages)
  "Display PACKAGES in the current info buffer.
PACKAGES should have a form of `guix-packages'."
  (guix-mapinsert (lambda (info)
                    (apply #'guix-info-insert-info
                           info guix-info-params))
                  packages
                  guix-info-delimiter)
  (goto-char (point-min)))

(defun guix-info-insert-info (info &rest params)
  "Insert (pretty print) package INFO into the current buffer.
Each element from PARAMS is a parameter to insert (symbol from
`guix-param-alist')."
  (mapc (lambda (param)
          (guix-info-insert-param
           param (guix-get-param-val param info)))
        params))

(defun guix-info-insert-param (param val)
  "Insert description and value VAL of a parameter PARAM at point.
PARAM is a symbol from `guix-param-alist'."
  (unless (and guix-info-ignore-empty-vals (null val))
    (let ((title (guix-get-param-title param))
          (insert-val (cdr (assq param guix-info-insert-param-alist))))
      (insert (format guix-info-param-title-format
                      (guix-get-string title 'guix-info-param-title)))
      (if (and val (functionp insert-val))
          (funcall insert-val val)
        (guix-info-insert-val
         val (and (facep insert-val) insert-val)))
      (insert "\n"))))

(defun guix-info-insert-location (location)
  "Make button from file LOCATION and insert it at point."
  (insert-button
   location
   'face 'guix-info-location
   'action (lambda (btn)
             (guix-find-location (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Find location of this package"))

(defun guix-info-insert-url (url)
  "Make button from URL and insert it at point."
  (insert-button
   url
   'face 'guix-info-url
   'action (lambda (btn) (browse-url (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Browse URL"))

(defun guix-info-insert-inputs (inputs)
  "Make buttons from INPUTS and insert those at point."
  (guix-info-insert-package-names inputs 'guix-info-inputs))

(defun guix-info-insert-native-inputs (inputs)
  "Make buttons from native INPUTS and insert those at point."
  (guix-info-insert-package-names inputs 'guix-info-native-inputs))

(defun guix-info-insert-package-names (names face)
  "Make buttons from package NAMES and insert those at point.
NAMES is a list of strings.
Propertize buttons with FACE."
  (guix-info-insert-val
   (with-temp-buffer
     (guix-mapinsert (lambda (name)
                       (guix-info-insert-package-name name face))
                     names
                     guix-list-separator)
     (buffer-substring (point-min) (point-max)))))

(defun guix-info-insert-package-name (name face)
  "Make button and insert package NAME at point.
Propertize package button with FACE."
  (insert-text-button
   name
   'face face
   'action (lambda (btn)
             (guix-info-get-show-packages 'name (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Describe the package"))

(defun guix-info-insert-val (val &optional face)
  "Format and insert parameter value at point.

If VAL is a one-line string longer than `guix-info-fill-column',
split it into several shorter lines.

If FACE is non-nil, propertize inserted line(s) with this FACE."
  (if (stringp val)
      (let ((strings (split-string val "\n *")))
        (and (null (cdr strings))       ; if not multi-line
             (> (length val) guix-info-fill-column)
             (setq strings
                   (split-string (guix-info-get-filled-string val)
                                 "\n")))
        (insert (guix-get-string (car strings) face))
        (guix-info-insert-strings (cdr strings) face))
    (insert (guix-get-string val face))))

(defun guix-info-get-filled-string (str &optional col)
  "Return string by filling a string STR.
COL controls the width for filling.
If COL is nil, use `guix-info-fill-column'."
  (or col (setq col guix-info-fill-column))
  (with-temp-buffer
    (insert str)
    (let ((fill-column col))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun guix-info-insert-strings (strings &optional face)
  "Insert STRINGS at point.
Each string is inserted on a new line after
`guix-info-multiline-prefix'.
If FACE is non-nil, propertize inserted lines with it."
  (dolist (str strings)
    (insert "\n" guix-info-multiline-prefix
            (guix-get-string str face))))

(provide 'guix-info)

;;; guix-info.el ends here
