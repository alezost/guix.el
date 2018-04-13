;;; guix-env-var.el --- Guix environment variables mode  -*- lexical-binding: t -*-

;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>

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

;; This file provides `guix-env-var-mode', the major mode for Guix
;; to prettify environment-variables and profile files.

;;; Code:

(defgroup guix-env-var nil
  "Settings for `guix-env-var-mode'."
  :prefix "guix-env-var-"
  :group  'guix)

(defcustom guix-env-var-enable-formatting t
  "If non-nil, prettify Guix environment-variables and profile files."
  :type 'boolean
  :group 'guix-env-var)

(defvar guix-env-var-display-message t
  "If non-nil, display a message after enabling `guix-env-var-mode'.
This message allows you to see that the current buffer is
formatted (while the original buffer file may not be formatted).")

(defvar guix-env-var-regexp
  (rx "export" (one-or-more space)
      letter (zero-or-more (or alphanumeric "_")) "=")
  "Regexp matching shell variables in Guix environment-variables file.")

(defvar guix-env-var-comment-line-regexp
  (rx (zero-or-more whitespace) "#")
  "Regexp matching a line beginning with a comment symbol.")

(defun guix-env-var-comment-line-p ()
  "Return non-nil if current line begins with a comment."
  (save-excursion
    (beginning-of-line)
    (re-search-forward guix-env-var-comment-line-regexp
                       (line-end-position) t)))

;;;###autoload
(defun guix-env-var-prettify-variable ()
  "Prettify shell environment variable at current line."
  (interactive)
  (let ((env-beg (line-beginning-position))
        (env-end (line-end-position)))
    (unless (guix-env-var-comment-line-p)
      (narrow-to-region env-beg env-end)
      (beginning-of-line)
      (when (search-forward "=" nil t)
        (insert "\\\n")
        (while (search-forward ":/" nil t)
          (backward-char 2)
          (insert "\\\n")
          (forward-char)))
      (end-of-line)
      (widen))))

;;;###autoload
(defun guix-env-var-prettify-buffer (&optional buffer)
  "Prettify BUFFER with `guix-env-var-prettify-variable'.
Interactively, or if BUFFER is not specified, use the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp guix-env-var-regexp nil t)
        (guix-env-var-prettify-variable)))))

;;;###autoload
(define-derived-mode guix-env-var-mode sh-mode "Guix-Env-Var"
  "Major mode for viewing Guix environment-variables and profile files.

\\{guix-env-var-mode-map}"
  (when guix-env-var-enable-formatting
    (let ((inhibit-read-only t))
      (guix-env-var-prettify-buffer (current-buffer)))
    (set-buffer-modified-p nil)
    (when guix-env-var-display-message
      (message "This buffer has been formatted by `guix-env-var-mode'."))))

(provide 'guix-env-var)

;;; guix-env-var.el ends here
