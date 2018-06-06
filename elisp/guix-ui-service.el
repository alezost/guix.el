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

;; This file provides 'list'/'info' interface for GuixSD services.

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
`name', `regexp', `location', `from-os-file', `from-expression'."
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
        (from-expression
         (message "%d services from '%s'."
                  count (car search-values)))
        (name
         (if (= 1 count)
             (message "'%s' service." (car search-values))
           (message "%d services with '%s' name."
                    count (car search-values))))
        (regexp
         (if (= 1 count)
             (message "A single service matching '%s'."
                      (car search-values))
           (message "%d services matching '%s'."
                    count (car search-values))))
        (location
         (if (= 1 count)
             (message "A single service placed in '%s'."
                      (car search-values))
           (message "%d services placed in '%s'."
                    count (car search-values))))
        (all
         (message "%d available services." count))))))


;;; Service 'info'

(bui-define-interface guix-service info
  :mode-name "Service-Info"
  :buffer-name "*Guix Service Info*"
  :get-entries-function 'guix-service-info-get-entries
  :format '((name nil (simple guix-service-info-heading))
            nil
            (description nil (simple guix-service-info-description))
            nil
            (location simple guix-service-info-insert-location)
            (extensions format (format guix-service-name))
            nil
            (shepherd simple guix-service-insert-shepherd-services))
  :titles '((shepherd . "Shepherd service(s)")))

(bui-define-interface guix-shepherd-service info
  :format '((names format (format))
            (documentation format (format guix-service-info-description))
            (requirements format (format)))
  :titles '((names . "Name(s)"))
  :reduced? t)

(defface guix-service-info-heading
  '((t :inherit bui-info-heading))
  "Face used for 'info' buffer heading (service name)."
  :group 'guix-service-info-faces)

(defface guix-service-info-description
  '((t :inherit font-lock-doc-face))
  "Face used for a description of a service."
  :group 'guix-service-info-faces)

(defface guix-service-info-extension
  '((t :inherit button))
  "Face used for service extensions."
  :group 'guix-service-info-faces)

(defvar guix-service-info-required-params
  '(id)
  "List of the required 'service' parameters.
These parameters are received from the Scheme side
along with the displayed parameters.

Do not remove `id' from this info as it is required for
identifying an entry.")

(define-button-type 'guix-service-name
  :supertype 'bui
  'face 'guix-service-info-extension
  'help-echo "Describe this service"
  'action (lambda (btn)
            (bui-get-display-entries-current
             'guix-service 'info
             (list 'name (button-label btn))
             'add)))

(defun guix-service-info-get-entries (search-type &rest search-values)
  "Return 'service' entries for displaying them in 'info' buffer."
  (guix-service-get-entries
   search-type search-values
   (cl-union guix-service-info-required-params
             (bui-info-displayed-params 'guix-service))))

(defun guix-service-info-insert-location (location &optional _)
  "Insert service LOCATION at point."
  (bui-insert-non-nil location
    (bui-info-insert-value-indent location 'guix-location)
    (let ((location-file (guix-location-file location)))
      (bui-insert-indent)
      (bui-insert-action-button
       "Services"
       (lambda (btn)
         (guix-service-get-display 'location
                                   (button-get btn 'location)))
       (format "Display services from location '%s'" location-file)
       'location location-file))))

(defvar guix-shepherd-service-info-delimiter
  (concat (make-string 16 ?—) "\n")
  "String used to separate shepherd services.")

(defun guix-service-insert-shepherd-services (shepherd-services _)
  "Insert SHEPHERD-SERVICES info at point."
  (bui-insert-non-nil shepherd-services
    (bui-newline)
    (bui-mapinsert
     (lambda (service)
       (bui-info-insert-entry service 'guix-shepherd-service 1))
     shepherd-services
     (concat (bui-get-indent 1)
             guix-shepherd-service-info-delimiter))))


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

(defun guix-service-list-describe (&rest ids)
  "Describe services with IDS (list of identifiers)."
  (bui-get-display-entries 'guix-service 'info (cons 'id ids)))

(defun guix-service-list-edit (&optional directory)
  "Go to the location of the current service.
See `guix-find-location' for the meaning of DIRECTORY."
  (interactive (list (guix-read-directory)))
  (guix-find-location (bui-entry-value (bui-list-current-entry)
                                       'location)
                      directory))


;;; Interactive commands

(defvar guix-service-search-params '(name description)
  "Default list of service parameters for searching by regexp.")

(defvar guix-service-search-history nil
  "A history of minibuffer prompts.")

(defvar guix-default-services-variables
  '(("%base-services"    . "(gnu services base)")
    ("%desktop-services" . "(gnu services desktop)"))
  "Alist of variables with services and their modules.
Each element from this alist should have the following form:

  (VAR-NAME . MODULE)

VAR-NAME is the name (string) of a guile variable that evaluates
to a list of services.

MODULE is the guile module (string) where this variable is placed in.")

;;;###autoload
(defun guix-services-from-system-config-file (file)
  "Display Guix services from the operating system configuration FILE.
See `guix-packages-from-system-config-file' for more details on FILE.
Interactively, prompt for FILE (see also `guix-support-dired')."
  (interactive (list (guix-read-os-file-name)))
  (guix-service-get-display 'from-os-file file))

;;;###autoload
(defun guix-all-services ()
  "Display all available Guix services."
  (interactive)
  (guix-service-get-display 'all))

;;;###autoload
(defun guix-default-services (var-name)
  "Display Guix services from VAR-NAME.
VAR-NAME is a name of the variable from
`guix-default-services-variables'."
  (interactive
   (list (completing-read
          "Variable with services: "
          guix-default-services-variables nil t nil nil
          (caar guix-default-services-variables))))
  (let ((module (bui-assoc-value guix-default-services-variables
                                 var-name)))
    (if module
        (guix-service-get-display
         'from-expression
         (format "(@ %s %s)" module var-name))
      (error "Unknown guile variable '%s'.
Check the value of 'guix-default-services-variables'"
             var-name))))

;;;###autoload
(defun guix-services-by-name (name)
  "Display Guix service(s) with NAME."
  (interactive
   (list (guix-read-service-name)))
  (guix-service-get-display 'name name))

;;;###autoload
(defun guix-services-by-regexp (regexp &optional params)
  "Search for Guix services by REGEXP.
PARAMS are service parameters that should be searched.
If PARAMS are not specified, use `guix-service-search-params'."
  (interactive
   (list (read-regexp "Regexp: " nil 'guix-service-search-history)))
  (guix-service-get-display 'regexp regexp
                            (or params guix-service-search-params)))

;;;###autoload
(defun guix-services-by-location (location)
  "Display Guix services placed in LOCATION file."
  (interactive
   (list (guix-read-service-location-file)))
  (guix-service-get-display 'location location))

(provide 'guix-ui-service)

;;; guix-ui-service.el ends here
