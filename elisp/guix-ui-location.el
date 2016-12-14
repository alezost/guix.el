;;; guix-ui-location.el --- Interface for displaying package locations  -*- lexical-binding: t -*-

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

;; This file provides a 'list' interface for displaying locations of Guix
;; packages.

;;; Code:

(require 'bui)
(require 'guix)
(require 'guix-location)
(require 'guix-repl)
(require 'guix-utils)

(guix-define-groups location)

(defun guix-location-get-entries ()
  "Receive 'package location' entries."
  (guix-eval-read "(package-location-sexps)"))


;;; Location 'list'

(bui-define-interface guix-location list
  :buffer-name "*Guix Package Locations*"
  :get-entries-function 'guix-location-get-entries
  :format '((location guix-location-list-file-name-specification 50 t)
            (number-of-packages nil 10 bui-list-sort-numerically-1
                                :right-align t))
  :sort-key '(location))

(let ((map guix-location-list-mode-map))
  (define-key map (kbd "RET") 'guix-location-list-show-packages)
  (define-key map (kbd "P")   'guix-location-list-show-packages)
  ;; "Location Info" buffer is not defined (it would be useless), so
  ;; unbind "i" key (by default, it is used to display Info buffer).
  (define-key map (kbd "i") nil))

(defun guix-location-list-file-name-specification (location &optional _)
  "Return LOCATION button specification for `tabulated-list-entries'."
  (bui-get-non-nil location
    (list location
          :type 'bui-file
          'action (lambda (btn)
                    (guix-find-location (button-get btn 'location)))
          'help-echo (concat "Find location: " location)
          'location location)))

(declare-function guix-packages-by-location "guix-ui-package" t)

(defun guix-location-list-show-packages ()
  "Display packages placed in the location at point."
  (interactive)
  (guix-packages-by-location (bui-list-current-id)))


;;; Interactive commands

;;;###autoload
(defun guix-locations ()
  "Display locations of the Guix packages."
  (interactive)
  (bui-list-get-display-entries 'guix-location))

(provide 'guix-ui-location)

;;; guix-ui-location.el ends here
