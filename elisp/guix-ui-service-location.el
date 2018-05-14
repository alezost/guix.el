;;; guix-ui-service-location.el --- Interface for displaying service locations  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

;; This file provides a 'list' interface for displaying locations of
;; GuixSD services.

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-location)
(require 'guix-repl)
(require 'guix-utils)

(guix-define-groups service-location)

(defun guix-service-location-get-entries ()
  "Receive 'service location' entries."
  (guix-eval-read "(service-location-sexps)"))


;;; Location 'list'

(bui-define-interface guix-service-location list
  :mode-name "Location-List"
  :buffer-name "*Guix Service Locations*"
  :get-entries-function 'guix-service-location-get-entries
  :format '((location guix-location-list-specification 50 t)
            (number-of-services nil 10 bui-list-sort-numerically-1
                                :right-align t))
  :hint 'guix-service-location-list-hint
  :sort-key '(location))

(let ((map guix-service-location-list-mode-map))
  (define-key map (kbd "RET") 'guix-service-location-list-show-services)
  (define-key map (kbd "e")   'guix-service-location-list-edit)
  ;; "Location Info" buffer is not defined.
  (define-key map (kbd "i") nil))

(defvar guix-service-location-list-default-hint
  '(("\\[guix-service-location-list-show-services]") " show services;\n"
    ("\\[guix-service-location-list-edit]")
    " edit (go to) the location file;\n"))

(defun guix-service-location-list-hint ()
  (bui-format-hints
   guix-service-location-list-default-hint
   bui-list-sort-hint
   bui-common-hint))

(defun guix-service-location-list-edit ()
  "Go to the service location file at point."
  (interactive)
  (guix-find-location (bui-list-current-id)))

(declare-function guix-services-by-location "guix-ui-service" t)

(defun guix-service-location-list-show-services ()
  "Display services placed in the location at point."
  (interactive)
  (guix-services-by-location (bui-list-current-id)))


;;; Interactive commands

(defun guix-service-locations-show ()
  "Display locations of the Guix services.
Unlike `guix-service-locations', this command always recreates
`guix-service-location-list-buffer-name' buffer."
  (interactive)
  (bui-list-get-display-entries 'guix-service-location))

;;;###autoload
(defun guix-service-locations ()
  "Display locations of the Guix services.
Switch to the `guix-service-location-list-buffer-name' buffer if
it already exists."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-service-location-list-buffer-name
   #'guix-service-locations-show 'message))

(provide 'guix-ui-service-location)

;;; guix-ui-service-location.el ends here
