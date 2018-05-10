;;; guix-ui-service.el --- Interface for displaying services  -*- lexical-binding: t -*-

;; Copyright © 2017–2018 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public Service as published by
;; the Free Software Foundation, either version 3 of the Service, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public Service for more details.
;;
;; You should have received a copy of the GNU General Public Service
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/services/>.

;;; Commentary:

;; This file provides 'list' interface to display Guix services.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-read)
(require 'guix-location)

(guix-define-groups service)

(bui-define-entry-type guix-service
  :message-function 'guix-service-message)

(defun guix-service-get-entries (search-type search-values params)
  "Receive 'service' entries.
SEARCH-TYPE may be one of the following symbols: `id', `all',
`name', `from-os-file'."
  (guix-eval-read
   (guix-make-guile-expression
    'service-sexps search-type search-values params)))

(defun guix-service-get-display (search-type &rest search-values)
  "Search for services and show results."
  (apply #'bui-list-get-display-entries
         'guix-service search-type search-values))

(defun guix-service-message (entries search-type &rest search-values)
  "Display a message after showing service ENTRIES."
  (if (null entries)
      (message "Couldn't find services")
    (let ((count (length entries)))
      (cl-case search-type
        (from-os-file
         (message "%d services from OS file '%s'."
                  count (car search-values)))
        (name
         (if (= 1 count)
             (message "'%s' service." (car search-values))
           (message "%d services with '%s' name."
                    count (car search-values))))
        (all
         (message "%d available services." count))))))


;;; Service 'list'

(bui-define-interface guix-service list
  :mode-name "Service-List"
  :buffer-name "*Guix Services*"
  :get-entries-function 'guix-service-list-get-entries
  :describe-function 'guix-service-list-describe
  :format '((name nil 25 t)
            (location guix-location-list-specification 35 t)
            (description bui-list-get-one-line 30 t))
  :sort-key '(name)
  :hint 'guix-service-list-hint)

(defvar guix-service-list-required-params
  '(id)
  "List of the required 'service' parameters.
These parameters are received from the Scheme side
along with the displayed parameters.

Do not remove `id' from this list as it is required for
identifying an entry.")

(let ((map guix-service-list-mode-map))
  (define-key map (kbd "e") 'guix-service-list-edit))

(defvar guix-service-list-default-hint
  '(("\\[guix-service-list-edit]") " edit (go to) the service definition;\n"))

(defun guix-service-list-hint ()
  (bui-format-hints
   guix-service-list-default-hint
   (bui-default-hint)))

(defun guix-service-list-get-entries (search-type &rest search-values)
  "Return 'service' entries for displaying them in 'list' buffer."
  (guix-service-get-entries
   search-type search-values
   (cl-union guix-service-list-required-params
             (bui-list-displayed-params 'guix-service))))

(defun guix-service-list-edit (&optional directory)
  "Go to the location of the current service.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-find-location (bui-entry-value (bui-list-current-entry)
                                       'location)
                      directory))


;;; Interactive commands

;;;###autoload
(defun guix-services-from-system-config-file (file)
  "Display Guix services from the operating system configuration FILE.
See `guix-packages-from-system-config-file' for more details on FILE."
  (interactive
   (list (guix-read-file-name "System configuration file: ")))
  (guix-service-get-display 'from-os-file file))

;;;###autoload
(defun guix-all-services ()
  "Display all available Guix services."
  (interactive)
  (guix-service-get-display 'all))

;;;###autoload
(defun guix-services-by-name (name)
  "Display Guix service(s) with NAME."
  (interactive
   (list (guix-read-service-name)))
  (guix-service-get-display 'name name))

(provide 'guix-ui-service)

;;; guix-ui-service.el ends here
