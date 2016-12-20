;;; guix-config.el --- Configuration variables

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

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

;; This file provides configuration variables for Emacs-Guix package.
;; Mainly, it is a wrapper for either `guix-build-config' or
;; `guix-default-config'.  Why?  Well, when Emacs-Guix is built using
;; the GNU Build System (./configure and make), "guix-build-config.el"
;; file is generated, se we can use it.  But when Emacs-Guix is used
;; without building (e.g., from MELPA), we don't have this file, so
;; instead, there is "guix-default-config.el" with the same variables.

;;; Code:

(or (require 'guix-build-config nil t)
    (require 'guix-default-config))

;; Avoid compilation warnings.
(defvar guix-config-scheme-directory)

(defvar guix-scheme-directory
  ;; If `guix-config-scheme-directory' is nil, then Emacs-Guix is used
  ;; from source without building (i.e., from MELPA), so find Scheme
  ;; files in a relative directory.
  (or guix-config-scheme-directory
      (let* ((elisp-dir  (file-name-directory load-file-name))
             (scheme-dir (expand-file-name "scheme" elisp-dir)))
        (if (file-exists-p scheme-dir)
            scheme-dir
          (let ((scheme-dir (expand-file-name "../scheme" elisp-dir)))
            (if (file-exists-p scheme-dir)
                scheme-dir
              (message "WARNING: Can't define `guix-scheme-directory'!")
              nil)))))
  "Directory with Scheme files for Emacs-Guix package.
It should be a directory where Guile modules are placed, i.e. a
directory with 'emacs-guix' sub-directory.")

(provide 'guix-config)

;;; guix-config.el ends here
