;;; guix-hash.el --- Calculate hashes of files  -*- lexical-binding: t -*-

;; Copyright © 2017 Alex Kost <alezost@gmail.com>

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

;; This file provides the code to determine SHA256 hashes of files (these
;; hashes are used in Guix packages).

;;; Code:

(require 'guix-help-vars)
(require 'guix-read)
(require 'guix-repl)
(require 'guix-guile)

(defcustom guix-hash-support-dired t
  "Whether '\\[guix-hash]' supports `dired-mode' or not.

If non-nil, do not prompt for a file name in `dired-mode' and use
the file on the current line instead.

If nil, always prompt for a file name."
  :type 'boolean
  :group 'guix)

(declare-function dired-get-filename "dired" t)

;;;###autoload
(defun guix-hash (file &optional format)
  "Calculate and copy (put into `kill-ring') the hash of FILE.

If FILE is a directory, calculate its hash recursively excluding
version-controlled files.

Interactively, prompt for FILE (see also
`guix-hash-support-dired' variable).  With prefix argument,
prompt for FORMAT as well.

See also Info node `(guix) Invoking guix hash'."
  (interactive
   (list (if (and guix-hash-support-dired
                  (derived-mode-p 'dired-mode))
             (dired-get-filename)
           (read-file-name "File: "))
         (and current-prefix-arg
              (guix-read-hash-format))))
  (let* ((file (expand-file-name file))
         (args (list :format (or format guix-default-hash-format)))
         (args (if (file-directory-p file)
                   (append '(:recursive? t) args)
                 args))
         (hash (guix-eval-read
                (apply #'guix-make-guile-expression
                       'file-hash file args))))
    (kill-new hash)
    (message "Hash of \"%s\" (%s) has been added to the kill ring."
             (file-name-nondirectory (directory-file-name file))
             hash)))

(provide 'guix-hash)

;;; guix-hash.el ends here
