;;; guix-profiles.el --- Guix profiles

;; Copyright © 2014–2016 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>

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

;; This file provides a general code related to location and contents of
;; Guix profiles.

;;; Code:

(defvar guix-state-directory
  ;; guix-daemon honors `NIX_STATE_DIR'.
  (or (getenv "NIX_STATE_DIR") "/var/guix"))

(defvar guix-user-profile
  (expand-file-name "~/.guix-profile")
  "User profile.")

(defvar guix-system-profile
  (concat guix-state-directory "/profiles/system")
  "System profile.")

(defvar guix-default-profile
  (concat guix-state-directory
          "/profiles/per-user/"
          (getenv "USER")
          "/guix-profile")
  "Default Guix profile.")

(defvar guix-current-profile guix-default-profile
  "Current Guix profile.
It is used by various commands as the default working profile.")

(defvar guix-system-profile-regexp
  (concat "\\`" (regexp-quote guix-system-profile))
  "Regexp matching system profiles.")

(defun guix-system-profile? (profile)
  "Return non-nil, if PROFILE is a system one."
  (string-match-p guix-system-profile-regexp profile))

(defun guix-generation-file (profile generation)
  "Return the file name of a PROFILE's GENERATION."
  (format "%s-%s-link" profile generation))

(defun guix-packages-profile (profile &optional generation system?)
  "Return a directory where packages are installed for the
PROFILE's GENERATION.

If SYSTEM? is non-nil, then PROFILE is considered to be a system
profile.  Unlike usual profiles, for a system profile, packages
are placed in 'profile' subdirectory."
  (let ((profile (if generation
                     (guix-generation-file profile generation)
                   profile)))
    (if system?
        (expand-file-name "profile" profile)
      profile)))

(defun guix-manifest-file (profile &optional generation system?)
  "Return the file name of a PROFILE's manifest.
See `guix-packages-profile'."
  (expand-file-name "manifest"
                    (guix-packages-profile profile generation system?)))

(defun guix-profile-prompt (&optional default)
  "Prompt for profile and return it.
Use DEFAULT as a start directory.  If it is nil, use
`guix-current-profile'."
  (let* ((dir (read-file-name "Profile: "
                              (file-name-directory
                               (or default guix-current-profile))))
         (dir (directory-file-name (expand-file-name dir))))
    (if (string= dir guix-user-profile)
        guix-default-profile
      dir)))

;;;###autoload
(defun guix-set-current-profile (file-name)
  "Set `guix-current-profile' to FILE-NAME.
Interactively, prompt for FILE-NAME.  With prefix, use
`guix-default-profile'."
  (interactive
   (list (if current-prefix-arg
             guix-default-profile
           (guix-profile-prompt))))
  (setq guix-current-profile file-name)
  (message "Current profile has been set to '%s'."
           guix-current-profile))

(provide 'guix-profiles)

;;; guix-profiles.el ends here
