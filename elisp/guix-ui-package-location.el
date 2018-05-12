;;; guix-ui-package-location.el --- Interface for displaying package locations  -*- lexical-binding: t -*-

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

;; This file provides a 'list' interface for displaying locations of Guix
;; packages.

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-location)
(require 'guix-repl)
(require 'guix-utils)

(guix-define-groups package-location)

(defun guix-package-location-get-entries ()
  "Receive 'package location' entries."
  (guix-eval-read "(package-location-sexps)"))


;;; Location 'list'

(bui-define-interface guix-package-location list
  :mode-name "Location-List"
  :buffer-name "*Guix Package Locations*"
  :get-entries-function 'guix-package-location-get-entries
  :format '((location guix-location-list-specification 50 t)
            (number-of-packages nil 10 bui-list-sort-numerically-1
                                :right-align t))
  :hint 'guix-package-location-list-hint
  :sort-key '(location))

(let ((map guix-package-location-list-mode-map))
  (define-key map (kbd "RET") 'guix-package-location-list-show-packages)
  (define-key map (kbd "P")   'guix-package-location-list-show-packages)
  (define-key map (kbd "e")   'guix-package-location-list-edit)
  ;; "Location Info" buffer is not defined (it would be useless), so
  ;; unbind "i" key (by default, it is used to display Info buffer).
  (define-key map (kbd "i") nil))

(defvar guix-package-location-list-default-hint
  '(("\\[guix-package-location-list-show-packages]") " show packages;\n"
    ("\\[guix-package-location-list-edit]")
    " edit (go to) the location file;\n"))

(defun guix-package-location-list-hint ()
  (bui-format-hints
   guix-package-location-list-default-hint
   bui-list-sort-hint
   bui-common-hint))

(defun guix-package-location-list-edit ()
  "Go to the package location file at point."
  (interactive)
  (guix-find-location (bui-list-current-id)))

(declare-function guix-packages-by-location "guix-ui-package" t)

(defun guix-package-location-list-show-packages ()
  "Display packages placed in the location at point."
  (interactive)
  (guix-packages-by-location (bui-list-current-id)))


;;; Interactive commands

(defun guix-package-locations-show ()
  "Display locations of the Guix packages.
Unlike `guix-package-locations', this command always recreates
`guix-package-location-list-buffer-name' buffer."
  (interactive)
  (bui-list-get-display-entries 'guix-package-location))

;;;###autoload
(defun guix-package-locations ()
  "Display locations of the Guix packages.
Switch to the `guix-package-location-list-buffer-name' buffer if
it already exists."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-package-location-list-buffer-name
   #'guix-package-locations-show 'message))

;;;###autoload
(define-obsolete-function-alias 'guix-locations
  'guix-package-locations "0.4")

(provide 'guix-ui-package-location)

;;; guix-ui-package-location.el ends here
