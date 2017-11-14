;;; guix-scheme.el --- Scheme major mode  -*- lexical-binding: t -*-

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

;; This file provides `guix-scheme-mode'.

;;; Code:

(require 'scheme)
(require 'guix-utils)

(defvar guix-scheme-display-message t
  "If non-nil, display a message after enabling `guix-scheme-mode'.
This message allows you to see that the current buffer is
indented (while the original buffer file may not be indented).")

;;;###autoload
(define-derived-mode guix-scheme-mode scheme-mode "Scheme"
  "Major mode for editing Scheme code.
This mode is the same as `scheme-mode', except it also reindents
the current buffer after it is enabled."
  (guix-pretty-print-buffer)
  ;; There may be long lines (like lists of strings), so display them on
  ;; multiple lines.
  (setq truncate-lines nil)
  (when guix-scheme-display-message
    (message "This buffer has been indented by `guix-scheme-mode'.")))

(provide 'guix-scheme)

;;; guix-scheme.el ends here
