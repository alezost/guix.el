;;; guix-about.el --- Various info about Guix and Emacs-Guix

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

;; This file provides the code to display various info about Guix (e.g., its
;; version).

;;; Code:

(require 'guix-config)

(declare-function guix-eval-read "guix-repl" (str))

;;;###autoload
(defun guix-version ()
  "Display Emacs-Guix and Guix versions in the echo area."
  (interactive)
  (require 'guix-repl)
  (message "%s %s\n%s %s"
           (guix-eval-read "(@ (guix config) %guix-package-name)")
           (guix-eval-read "(@ (guix config) %guix-version)")
           guix-config-name
           guix-config-version))

(provide 'guix-about)

;;; guix-about.el ends here
