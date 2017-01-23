;;; guix-package.el --- Guix packages  -*- lexical-binding: t -*-

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>

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

;; This file provides a general code related to Guix package.

;;; Code:

(require 'guix-repl)
(require 'guix-guile)
(require 'guix-misc)

(defun guix-package-name-specification (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "@" version
          (when output (concat ":" output))))

(defun guix-package-id-and-output-by-output-id (output-id)
  "Return a list (PACKAGE-ID OUTPUT) by OUTPUT-ID."
  (cl-multiple-value-bind (package-id-str output)
      (split-string output-id ":")
    (let ((package-id (string-to-number package-id-str)))
      (list (if (= 0 package-id) package-id-str package-id)
            output))))

(defun guix-package-build-log-file (id)
  "Return build log file name of a package defined by ID."
  (guix-eval-read
   (guix-make-guile-expression 'package-build-log-file id)))

(declare-function guix-build-log-find-file "guix-build-log" (file))

(defun guix-package-find-build-log (id)
  "Show build log of a package defined by ID."
  (require 'guix-build-log)
  (let ((file (guix-package-build-log-file id)))
    (if file
        (guix-build-log-find-file file)
      (message "Couldn't find the package build log."))))

(defun guix-package-source-file-name (package-id)
  "Return a store file name to a source of a package PACKAGE-ID."
  (message "Calculating the source derivation ...")
  (guix-eval-read
   (guix-make-guile-expression
    'package-source-file-name package-id)))

(defun guix-package-store-path (package-id)
  "Return a list of store directories of outputs of package PACKAGE-ID."
  (message "Calculating the package derivation ...")
  (guix-eval-read
   (guix-make-guile-expression
    'package-store-path package-id)))

(defvar guix-after-source-download-hook nil
  "Hook run after successful performing a 'source-download' operation.")

(defun guix-package-source-build-derivation (package-id &optional prompt)
  "Build source derivation of a package PACKAGE-ID.
Ask a user with PROMPT for continuing an operation."
  (when (or (not guix-operation-confirm)
            (guix-operation-prompt (or prompt
                                       "Build the source derivation?")))
    (guix-eval-in-repl
     (guix-make-guile-expression
      'package-source-build-derivation
      package-id
      :use-substitutes? (or guix-use-substitutes 'f)
      :dry-run? (or guix-dry-run 'f))
     nil 'source-download)))

(defun guix-build-package (package-id &optional prompt)
  "Build package with PACKAGE-ID.
Ask a user with PROMPT for continuing the build operation."
  (when (or (not guix-operation-confirm)
            (guix-operation-prompt (or prompt "Build package?")))
    (guix-eval-in-repl
     (format (concat "(build-package* (package-by-id %d)"
                     " #:use-substitutes? %s"
                     " #:dry-run? %s)")
             package-id
             (guix-guile-boolean guix-use-substitutes)
             (guix-guile-boolean guix-dry-run)))))

(provide 'guix-package)

;;; guix-package.el ends here
