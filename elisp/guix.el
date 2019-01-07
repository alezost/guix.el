;;; guix.el --- Interface for GNU Guix

;; Copyright © 2016–2019 Alex Kost <alezost@gmail.com>

;; Author: Alex Kost <alezost@gmail.com>
;; Version: 0.5.1.1
;; URL: https://emacs-guix.gitlab.io/website/
;; Keywords: tools
;; Package-Requires: ((emacs "24.3") (dash "2.11.0") (geiser "0.8") (bui "1.2.0") (magit-popup "2.1.0") (edit-indirect "0.1.4"))

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

;; Emacs-Guix (aka "guix.el") provides featureful visual interface for
;; the GNU Guix package manager.  It allows you:
;;
;; - to search for packages and to look at their code (package recipes);
;;
;; - to manage your Guix profile(s) by installing/removing packages;
;;
;; - to look at, compare and remove profile generations;
;;
;; - to look at system services and generations (if you use GuixSD);
;;
;; - to do many other things.
;;
;; Run "M-x guix-help" to look at the summary of available commands.

;;; Code:

(defgroup guix nil
  "Interface for the GNU Guix package manager."
  :prefix "guix-"
  :group 'external)

(defgroup guix-faces nil
  "Guix faces."
  :group 'guix
  :group 'faces)

(defface guix-true
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Parent face for various true-kind things ('Yes' strings, etc.)."
  :group 'guix-faces)

(defface guix-false
  '((t :weight bold))
  "Parent face for various false-kind things ('No' strings, etc.)."
  :group 'guix-faces)

(provide 'guix)

;;; guix.el ends here
