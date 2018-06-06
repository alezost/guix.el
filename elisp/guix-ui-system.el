;;; guix-ui-system.el --- Interface for 'operating-system' declaration  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public System as published by
;; the Free Software Foundation, either version 3 of the System, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public System for more details.
;;
;; You should have received a copy of the GNU General Public System
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/systems/>.

;;; Commentary:

;; This file provides 'info' interface for GuixSD system configuration
;; files – i.e., for 'operating-system' declaration.

;;; Code:

(require 'cl-lib)
(require 'bui)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-profiles)
(require 'guix-utils)
(require 'guix-ui-package)

(guix-define-groups system)

(bui-define-entry-type guix-system
  :titles '((number-of-packages . "Packages")
            (number-of-services . "Services")))

(defun guix-system-get-entries (search-type search-values params)
  "Receive 'system' entries.
SEARCH-TYPE may be one of the following symbols: `from-file'."
  (let ((sexps (guix-eval-read
                (guix-make-guile-expression
                 'system-sexps search-type search-values params))))
    (if (eq search-type 'from-file)
        (mapcar (lambda (alist)
                  (cons `(file . ,(car search-values))
                        alist))
                sexps)
      sexps)))

(defun guix-system-get-display (search-type &rest search-values)
  "Search for systems and show results."
  (bui-get-display-entries
   'guix-system 'info (cl-list* search-type search-values)))


;;; System 'info'

(bui-define-interface guix-system info
  :mode-name "System-Info"
  :buffer-name "*Guix System Info*"
  :get-entries-function 'guix-system-info-get-entries
  :format '((file nil (simple bui-file))
            nil
            (number-of-packages format
                                guix-system-info-insert-number-of-packages)
            (number-of-services format
                                guix-system-info-insert-number-of-services)
            (kernel format (guix-package-info-insert-name-button))
            (bootloader format (guix-package-info-insert-name-button))
            (firmware format (guix-package-info-insert-name-buttons))
            (initrd-modules format (format))))

(defun guix-system-info-get-entries (search-type &rest search-values)
  "Return 'system' entries for displaying them in 'info' buffer."
  (guix-system-get-entries
   search-type search-values
   (bui-info-displayed-params 'guix-system)))

(defun guix-system-info-insert-number-of-packages (number entry)
  "Insert the NUMBER of packages and button to display packages."
  (bui-format-insert number)
  (bui-insert-indent)
  (bui-insert-action-button
   "Show"
   (lambda (btn)
     (guix-packages-from-system-config-file (button-get btn 'file)))
   "Show packages from this system"
   'file (bui-entry-non-void-value entry 'file)))

(declare-function guix-services-from-system-config-file
                  "guix-ui-service" t)

(defun guix-system-info-insert-number-of-services (number entry)
  "Insert the NUMBER of services and button to display services."
  (bui-format-insert number)
  (bui-insert-indent)
  (bui-insert-action-button
   "Show"
   (lambda (btn)
     (guix-services-from-system-config-file (button-get btn 'file)))
   "Show services from this system"
   'file (bui-entry-non-void-value entry 'file)))


;;; Interactive commands

;;;###autoload
(defun guix-system-from-file (file)
  "Display info on 'operating-system' declaration from FILE.
See `guix-packages-from-system-config-file' for more details on FILE.
Interactively, prompt for FILE (see also `guix-support-dired')."
  (interactive (list (guix-read-os-file-name)))
  (guix-system-get-display 'from-file file))

(provide 'guix-ui-system)

;;; guix-ui-system.el ends here
