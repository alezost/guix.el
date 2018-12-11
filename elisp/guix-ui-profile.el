;;; guix-ui-profile.el --- Interface for displaying profiles  -*- lexical-binding: t -*-

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

;; This file provides a 'list' interface for displaying Guix profiles
;; with `guix-profiles' command.
;;
;; `guix-profiles' variable controls what profiles are displayed.

;;; Code:

(require 'dash)
(require 'bui)
(require 'guix nil t)
(require 'guix-profiles)
(require 'guix-read)
(require 'guix-utils)
(require 'guix-misc)

(guix-define-groups profile)

(bui-define-entry-type guix-profile
  :get-entries-function 'guix-profile-get-entries
  :message-function 'guix-profile-message
  :titles '((number-of-packages    . "Packages")
            (number-of-generations . "Generations")))

(defcustom guix-profiles
  (--filter (and it (file-exists-p it))
            (delete-dups
             (list guix-user-profile
                   guix-system-profile
                   guix-pulled-profile
                   (--when-let (getenv "GUIX_PROFILE")
                     (guix-file-name it)))))
  "List of profiles displayed by '\\[guix-profiles]' command."
  :type '(repeat file)
  :group 'guix-profile)

(defun guix-profile->entry (profile)
  "Return 'guix-profile' entry by PROFILE file-name."
  (let* ((profile (guix-profile profile))
         (number-of-packages (guix-profile-number-of-packages
                              profile)))
    (if number-of-packages
        `((id                    . ,profile)
          (profile               . ,profile)
          (current               . ,(guix-current-profile? profile))
          (number-of-packages    . ,number-of-packages)
          (number-of-generations . ,(guix-profile-number-of-generations
                                     profile)))
      (error "No packages in '%s'.  Is it a real profile?" profile))))

(defun guix-profile-get-entries (&optional search-type &rest args)
  "Return 'guix-profile' entries."
  (let ((profiles (cond
                   ((or (null search-type)
                        (eq search-type 'all))
                    guix-profiles)
                   ((memq search-type '(id profile file-name))
                    args)
                   (t (error "Wrong search-type: %S" search-type)))))
    (mapcar #'guix-profile->entry profiles)))

(defun guix-profile-message (entries &rest _)
  "Display a message after showing profile ENTRIES."
  (unless entries
    (message "Guix profiles not found.  Set `guix-profiles' variable.")))

(defun guix-read-profile-from-entries (&optional entries)
  "Return profile file name from ENTRIES (current entries by default).
If there is only one entry, return its profile name.  If there
are multiple entries, prompt for a profile name and return it."
  (or entries (setq entries (bui-current-entries)))
  (if (cdr entries)
      (completing-read "Profile: "
                       (--map (bui-entry-value it 'profile)
                              entries))
    (bui-entry-value (car entries) 'profile)))


;;; Profile 'list'

(bui-define-interface guix-profile list
  :mode-name "Profile-List"
  :buffer-name "*Guix Profiles*"
  :describe-function 'guix-profile-list-describe
  :format '((current guix-profile-list-get-current 10 t)
            (profile bui-list-get-file-name 40 t)
            (number-of-packages nil 11 bui-list-sort-numerically-2
                                :right-align t)
            (number-of-generations nil 14 bui-list-sort-numerically-3
                                   :right-align t))
  :hint 'guix-profile-list-hint
  :sort-key '(profile))

(let ((map guix-profile-list-mode-map))
  (define-key map (kbd "E") 'guix-profile-list-show-search-paths)
  (define-key map (kbd "P") 'guix-profile-list-show-packages)
  (define-key map (kbd "G") 'guix-profile-list-show-generations)
  (define-key map (kbd "M") 'guix-profile-list-apply-manifest)
  (define-key map (kbd "c") 'guix-profile-list-set-current))

(defvar guix-profile-list-default-hint
  '(("\\[guix-profile-list-show-packages]") " show packages;\n"
    ("\\[guix-profile-list-show-generations]") " show generations;\n"
    ("\\[guix-profile-list-show-search-paths]") " show search paths;\n"
    ("\\[guix-profile-list-set-current]") " set current profile;\n"
    ("\\[guix-profile-list-apply-manifest]") " apply manifest;\n"))

(defun guix-profile-list-hint ()
  (bui-format-hints
   guix-profile-list-default-hint
   (bui-default-hint)))

(defun guix-profile-list-describe (&rest ids)
  "Describe profiles with IDS (list of identifiers)."
  (bui-display-entries
   (bui-entries-by-ids (bui-current-entries) ids)
   'guix-profile 'info (cons 'id ids)))

(defun guix-profile-list-current-profile ()
  "Return file name of the current profile."
  ;; (bui-entry-value (bui-list-current-entry) 'profile)
  ;; Just get the ID, as currently ID is the profile file name.
  (bui-list-current-id))

(defun guix-profile-list-marked-profiles ()
  "Return a list of file names of the marked profiles.
If nothing is marked, return a list with profile at point."
  (bui-list-marked-or-current))

(declare-function guix-installed-packages "guix-ui-package" t)
(declare-function guix-generations "guix-ui-generation" t)

(defun guix-profile-list-show-packages ()
  "Display packages installed in the current profile."
  (interactive)
  (guix-installed-packages (guix-profile-list-current-profile)))

(defun guix-profile-list-show-generations ()
  "Display generations of the current profile."
  (interactive)
  (guix-generations (guix-profile-list-current-profile)))

(defun guix-profile-list-show-search-paths (&optional type)
  "Display 'search paths' environment variables for the marked profiles.
If nothing is marked, use profile on the current line."
  (interactive (list (guix-read-search-paths-type)))
  (guix-show-search-paths (guix-profile-list-marked-profiles) type))

(defun guix-profile-list-apply-manifest (file)
  "Apply manifest from FILE to the current profile."
  (interactive (list (guix-read-manifest-file-name)))
  (guix-apply-manifest (guix-package-profile
                        (guix-profile-list-current-profile))
                       file (current-buffer)))

(defun guix-profile-list-get-current (value &optional _)
  "Return string from VALUE showing whether this profile is current."
  (if value "(current)" ""))

(defun guix-profile-list-set-current ()
  "Set `guix-current-profile' to the profile on the current line."
  (interactive)
  (guix-set-current-profile (guix-profile-list-current-profile))
  ;; Now updating "Current" column is needed.  It can be done simply by
  ;; reverting the buffer, but it should be more effective to reset
  ;; 'current' parameter for all entries and to redisplay the buffer
  ;; instead.
  (let* ((current-id  (bui-list-current-id))
         (new-entries (mapcar
                       (lambda (entry)
                         (let ((id (bui-entry-id entry)))
                           (cons `(current . ,(equal id current-id))
                                 (--remove-first (eq (car it) 'current)
                                                 entry))))
                       (bui-current-entries))))
    (setf (bui-item-entries bui-item)
          new-entries))
  (bui-redisplay))


;;; Profile 'info'

(bui-define-interface guix-profile info
  :mode-name "Profile-Info"
  :buffer-name "*Guix Profile Info*"
  :format '((profile nil (simple bui-file))
            nil
            guix-profile-info-insert-buttons
            (current format guix-profile-info-insert-current)
            (number-of-packages
             format guix-profile-info-insert-number-of-packages)
            (number-of-generations
             format guix-profile-info-insert-number-of-generations))
  :hint 'guix-profile-info-hint)

(let ((map guix-profile-info-mode-map))
  (define-key map (kbd "E") 'guix-profile-info-show-search-paths)
  (define-key map (kbd "P") 'guix-profile-info-show-packages)
  (define-key map (kbd "G") 'guix-profile-info-show-generations)
  (define-key map (kbd "M") 'guix-profile-info-apply-manifest)
  (define-key map (kbd "c") 'guix-profile-info-set-current))

(defvar guix-profile-info-default-hint
  '(("\\[guix-profile-info-show-packages]") " show packages;\n"
    ("\\[guix-profile-info-show-generations]") " show generations;\n"
    ("\\[guix-profile-info-show-search-paths]") " show search paths;\n"
    ("\\[guix-profile-info-set-current]") " set current profile;\n"
    ("\\[guix-profile-info-apply-manifest]") " apply manifest;\n"))

(defun guix-profile-info-hint ()
  (bui-format-hints
   guix-profile-info-default-hint
   (bui-default-hint)))

(defface guix-profile-info-current
  '((t :inherit guix-true))
  "Face used if a profile is the current one."
  :group 'guix-profile-info-faces)

(defface guix-profile-info-not-current
  '((t :inherit guix-false))
  "Face used if a profile is not the current one."
  :group 'guix-profile-info-faces)

(defun guix-profile-info-insert-search-paths-button (profile)
  "Insert 'Search paths' button for PROFILE."
  (bui-insert-action-button
   "Search paths"
   (lambda (btn)
     (guix-show-search-paths (list (button-get btn 'profile))
                             (guix-read-search-paths-type)))
   (format "Show 'search paths' environment variables for profile '%s'"
           profile)
   'profile profile))

(defun guix-profile-info-insert-apply-manifest-button (profile)
  "Insert 'Apply manifest' button for PROFILE."
  (bui-insert-action-button
   "Apply manifest"
   (lambda (btn)
     (guix-apply-manifest (button-get btn 'profile)
                          (guix-read-manifest-file-name)
                          (current-buffer)))
   (format "Apply manifest file to profile '%s'"
           profile)
   'profile profile))

(defun guix-profile-info-insert-buttons (entry)
  "Insert some buttons for profile ENTRY at point."
  (let ((profile (bui-entry-non-void-value entry 'profile)))
    (guix-profile-info-insert-search-paths-button profile)
    (unless (guix-system-profile? profile)
      (bui-insert-indent)
      (guix-profile-info-insert-apply-manifest-button profile))
    (bui-newline)))

(defun guix-profile-info-insert-current (value entry)
  "Insert boolean VALUE showing whether this profile is current."
  (if value
      (bui-info-insert-value-format "Yes" 'guix-profile-info-current)
    (bui-info-insert-value-format "No" 'guix-profile-info-not-current)
    (bui-insert-indent)
    (let ((profile (bui-entry-non-void-value entry 'profile)))
      (bui-insert-action-button
       "Set"
       (lambda (btn)
         (guix-set-current-profile (button-get btn 'profile))
         (bui-revert nil t))
       (format "Make '%s' the current profile" profile)
       'profile profile))))

(defun guix-profile-info-insert-number-of-packages (number entry)
  "Insert the NUMBER of packages and button to display packages."
  (bui-format-insert number)
  (bui-insert-indent)
  (let ((profile (bui-entry-non-void-value entry 'profile)))
    (bui-insert-action-button
     "Show"
     (lambda (btn)
       (guix-installed-packages (button-get btn 'profile)))
     (format "Show packages installed in profile '%s'" profile)
     'profile profile)))

(defun guix-profile-info-insert-number-of-generations (number entry)
  "Insert the NUMBER of generations and button to display generations."
  (bui-format-insert number)
  (bui-insert-indent)
  (let ((profile (bui-entry-non-void-value entry 'profile)))
    (bui-insert-action-button
     "Show"
     (lambda (btn)
       (guix-generations (button-get btn 'profile)))
     (format "Show generations of profile '%s'" profile)
     'profile profile)))

(defun guix-profile-info-show-packages (profile)
  "Display packages installed in PROFILE."
  (interactive (list (guix-read-profile-from-entries)))
  (guix-installed-packages profile))

(defun guix-profile-info-show-generations (profile)
  "Display generations of PROFILE."
  (interactive (list (guix-read-profile-from-entries)))
  (guix-generations profile))

(defun guix-profile-info-show-search-paths (profile &optional type)
  "Display 'search paths' environment variables for PROFILE."
  (interactive
   (list (guix-read-profile-from-entries)
         (guix-read-search-paths-type)))
  (guix-show-search-paths (list profile) type))

(defun guix-profile-info-apply-manifest (profile &optional file)
  "Apply manifest from FILE to PROFILE."
  (interactive
   (list (guix-read-profile-from-entries)
         (guix-read-manifest-file-name)))
  (guix-apply-manifest profile file (current-buffer)))

(defun guix-profile-info-set-current (profile)
  "Set `guix-current-profile' to PROFILE."
  (interactive (list (guix-read-profile-from-entries)))
  (guix-set-current-profile profile)
  (bui-revert nil t))


;;; Interactive commands

(defun guix-profiles-show ()
  "Display Guix profiles.
Unlike `guix-profiles', this command always recreates
`guix-profile-list-buffer-name' buffer."
  (interactive)
  (bui-list-get-display-entries 'guix-profile))

;;;###autoload
(defun guix-profiles ()
  "Display Guix profiles.
Switch to the `guix-profile-list-buffer-name' buffer if it
already exists.

Modify `guix-profiles' variable to add more profiles."
  (interactive)
  (guix-switch-to-buffer-or-funcall
   guix-profile-list-buffer-name #'guix-profiles-show 'message))

;;;###autoload
(defun guix-system-profile ()
  "Display interface for `guix-system-profile'."
  (interactive)
  (bui-get-display-entries 'guix-profile 'info
                           (list 'profile guix-system-profile)))

;;;###autoload
(defun guix-current-profile ()
  "Display interface for `guix-current-profile'."
  (interactive)
  (bui-get-display-entries 'guix-profile 'info
                           (list 'profile guix-current-profile)))

(provide 'guix-ui-profile)

;;; guix-ui-profile.el ends here
