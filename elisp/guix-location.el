;;; guix-location.el --- Package and service locations  -*- lexical-binding: t -*-

;; Copyright © 2016–2018 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides the code to work with locations of Guix packages
;; and services.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'guix-repl)
(require 'guix-guile)

(defface guix-location
  '((t :inherit bui-file-name))
  "Face used for locations of packages and services."
  :group 'guix-faces)

(define-button-type 'guix-location
  :supertype 'bui
  'face 'guix-location
  'help-echo "Find this location"
  'action (lambda (btn)
            (guix-find-location (or (button-get btn 'location)
                                    (button-label btn)))))

(defun guix-location-list-specification (location &optional _)
  "Return LOCATION button specification for `tabulated-list-entries'."
  (bui-get-non-nil location
    (list location
          :type 'guix-location
          'location location)))

(defun guix-location-file (location)
  "Return file name of the LOCATION."
  (car (split-string location ":")))

(defun guix-find-location (location &optional directory)
  "Go to LOCATION.
LOCATION is a string of the form:

  \"FILE:LINE:COLUMN\"

If FILE is relative, it is considered to be relative to
DIRECTORY (if it is specified and exists)."
  (cl-multiple-value-bind (file line column)
      (split-string location ":")
    (let* ((file-name (expand-file-name file (or directory
                                                 (guix-directory))))
           (file-name (if (file-exists-p file-name)
                          file-name
                        (guix-eval-read
                         (guix-make-guile-expression
                          'search-load-path file)))))
      (unless file-name         ; not found in Guile %load-path
        (error "Location file not found: %s" file))
      (find-file file-name))
    (when (and line column)
      (let ((line   (string-to-number line))
            (column (string-to-number column)))
        (goto-char (point-min))
        (forward-line (- line 1))
        (move-to-column column)
        (recenter 1)))))

(provide 'guix-location)

;;; guix-location.el ends here
