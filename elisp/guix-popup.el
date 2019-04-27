;;; guix-popup.el --- Popup interface for Emacs-Guix commands

;; Copyright © 2018–2019 Alex Kost <alezost@gmail.com>

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

;; This file provides popup interface (using `magit-popup' library) for
;; Emacs-Guix commands.

;;; Code:

(require 'magit-popup)
(require 'guix-profiles)

(defgroup guix-popup nil
  "Popup interface for Emacs-Guix commands."
  :group 'guix)

;;;###autoload (autoload 'guix-popup "guix-popup" nil t)
(magit-define-popup guix-popup
  "Show popup buffer for Emacs-Guix commands."
  'guix-popup
  :actions '("Sub-popups"
             (?p "packages" guix-package-popup)
             (?P "profiles" guix-profile-popup)
             (?s "services" guix-service-popup)
             (?y "system commands" guix-system-popup)
             (?l "package licenses" guix-license-popup)
             (?S "store" guix-store-popup)
             (?m "major/minor modes" guix-mode-popup)
             (?c "guix shell commands" guix-command)
             "Miscellaneous commands"
             (?H "calculate file hash" guix-hash)
             (?E "set Emacs environment" guix-set-emacs-environment)
             "Auxiliary commands"
             (?a "about" guix-about)
             (?h "help (\"refcard\")" guix-help)
             (?i "info manual" guix-info)
             (?v "version" guix-version)
             (?b "switch to buffer" guix-switch-to-buffer)
             (?B "report guix bug" guix-report-bug))
  :max-action-columns #'guix-popup-max-columns)

(defun guix-popup-max-columns (heading)
  "Return the number of `:max-action-columns' for HEADING.
This function is used by command `guix-popup'."
  (pcase heading
    ("Sub-popups" 1)
    ("Miscellaneous commands" 1)
    (_ 2)))

;;;###autoload
(defalias 'guix #'guix-popup
  "Popup interface for Emacs-Guix commands.")

(defun guix-popup-variable-value (var-name)
  "Return string formatted for popup buffer.
String is made of variable VAR-NAME and its value."
  (concat (propertize (symbol-name var-name)
                      'face font-lock-variable-name-face)
          " "
          (propertize (prin1-to-string (symbol-value var-name))
                      'face 'magit-popup-option-value)))

(defun guix-popup-format-profile ()
  "Return profile, formatted for '\\[guix-popup]'."
  (guix-popup-variable-value 'guix-current-profile))

(defvar guix-popup-profile-variable
  '(?p "profile"
       guix-set-current-profile
       guix-popup-format-profile)
  "Popup structure for variable `guix-current-profile'.")


;;; Sub-popups

(magit-define-popup guix-package-popup
  "Show popup buffer for package commands."
  'guix-package-popup
  :variables (list guix-popup-profile-variable)
  :actions '("Show packages"
             (?a "all" guix-all-packages)
             (?i "installed" guix-installed-packages)
             (?o "obsolete" guix-obsolete-packages)
             (?s "superseded" guix-superseded-packages)
             "Search for packages"
             (?n "by name" guix-packages-by-name)
             (?N "by regexp (in name only)" guix-search-by-name)
             (?r "by regexp (in name, synopsis, description)"
                 guix-search-by-regexp)
             (?L "by location" guix-packages-by-location)
             (?c "by license" guix-packages-by-license)
             (?d "depending on other package(s)" guix-dependent-packages)
             (?f "packages from file" guix-package-from-file)
             (?y "packages from system config file"
                 guix-packages-from-system-config-file)
             "Package locations"
             (?l "show package locations" guix-package-locations)
             (?e "\"edit\" package (find package definition)"
                 guix-find-package-definition)
             (?F "find location file" guix-find-package-location-file)
             "Other commands"
             (?g "package graph" guix-package-graph)
             (?z "package size" guix-package-size)
             (?t "package lint" guix-package-lint)
             (?T "total number of packages" guix-number-of-packages))
  :max-action-columns #'guix-package-popup-max-columns)

(defun guix-package-popup-max-columns (heading)
  "Return the number of `:max-action-columns' for HEADING.
This function is used by command `guix-package-popup'."
  (pcase heading
    ("Show packages" 2)
    ("Other commands" 2)
    (_ 1)))

(magit-define-popup guix-profile-popup
  "Show popup buffer for profiles and generations commands."
  'guix-profile-popup
  :variables (list guix-popup-profile-variable)
  :actions '("Show profiles"
             (?a "all" guix-profiles)
             (?s "system" guix-system-profile)
             (?c "current" guix-current-profile)
             "Show generations (of the current profile)"
             (?g "all" guix-generations)
             (?t "by time" guix-generations-by-time)
             (?l "last" guix-last-generations)
             "Other commands"
             (?M "apply manifest to the current profile"
                 guix-apply-manifest))
  :max-action-columns 1)

(magit-define-popup guix-service-popup
  "Show popup buffer for service commands."
  'guix-service-popup
  :actions '("Show services"
             (?a "all" guix-all-services)
             (?d "default" guix-default-services)
             (?n "by name" guix-services-by-name)
             (?r "by regexp" guix-services-by-regexp)
             (?L "by location" guix-services-by-location)
             (?y "services from system config file"
                 guix-services-from-system-config-file)
             "Service locations"
             (?l "show service locations" guix-service-locations)
             (?e "\"edit\" service (find service definition)"
                 guix-find-service-definition)
             (?F "find location file" guix-find-service-location-file))
  :max-action-columns 1)

(magit-define-popup guix-system-popup
  "Show popup buffer for system commands."
  'guix-system-popup
  :actions '("From system profile"
             (?p "packages" guix-installed-system-packages)
             (?P "profile" guix-system-profile)
             (?g "all generations" guix-system-generations)
             (?t "generations by time" guix-system-generations-by-time)
             (?l "last generations" guix-last-system-generations)
             "From system configuration file"
             (?y "system" guix-system-from-file)
             (?k "packages" guix-packages-from-system-config-file)
             (?s "services" guix-services-from-system-config-file))
  :max-action-columns 1)

(magit-define-popup guix-license-popup
  "Show popup buffer for license commands."
  'guix-license-popup
  :actions '((?a "show all package licenses" guix-licenses)
             (?u "browse license URL" guix-browse-license-url)
             (?e "\"edit\" license (find license definition)"
                 guix-find-license-definition)
             (?F "find license location file"
                 guix-find-license-location-file))
  :max-action-columns 1)

(magit-define-popup guix-store-popup
  "Show popup buffer for store commands."
  'guix-store-popup
  :actions '("Show store items"
             (?l "live items" guix-store-live-items)
             (?d "dead items" guix-store-dead-items)
             (?e "failures" guix-store-failures)
             (?i "single item" guix-store-item)
             (?D "derivers" guix-store-item-derivers)
             (?R "requisites" guix-store-item-requisites)
             (?f "referrers" guix-store-item-referrers)
             (?F "references" guix-store-item-references))
  :max-action-columns 2)

(magit-define-popup guix-mode-popup
  "Show popup buffer for Emacs-Guix major/minor modes."
  'guix-mode-popup
  :actions '("Modes"
             (?p "guix-prettify-mode" guix-prettify-mode)
             (?P "global-guix-prettify-mode" global-guix-prettify-mode)
             (?b "guix-build-log-minor-mode" guix-build-log-minor-mode)
             (?B "guix-build-log-mode" guix-build-log-mode)
             (?d "guix-devel-mode" guix-devel-mode)
             (?D "guix-derivation-mode" guix-derivation-mode)
             (?e "guix-env-var-mode" guix-env-var-mode))
  :max-action-columns 1)

(provide 'guix-popup)

;;; guix-popup.el ends here
