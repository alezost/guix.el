;;; guix-ui-profile.el --- Interface for displaying profiles  -*- lexical-binding: t -*-

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

;; This file provides a 'list' interface for displaying Guix profiles
;; with `guix-profiles' command.
;;
;; `guix-profiles' variable controls what profiles are displayed.

;;; Code:

(require 'dash)
(require 'bui)
(require 'guix-profiles)
(require 'guix-utils)

(guix-define-groups profile)

(defcustom guix-profiles
  (-filter #'file-exists-p
           (list guix-user-profile
                 guix-system-profile))
  "List of profiles displayed by '\\[guix-profiles]' command."
  :type '(repeat file)
  :group 'guix-profile)

(defun guix-profile->entry (profile)
  "Return 'guix-profile' entry by PROFILE file-name."
  `((id                    . ,profile)
    (profile               . ,profile)
    (number-of-packages    . ,(guix-profile-number-of-packages
                               profile))
    (number-of-generations . ,(guix-profile-number-of-generations
                               profile))))

(defun guix-profile-get-entries ()
  "Return 'guix-profile' entries."
  (mapcar #'guix-profile->entry guix-profiles))


;;; Profile 'list'

(bui-define-interface guix-profile list
  :buffer-name "*Guix Profiles*"
  :get-entries-function 'guix-profile-get-entries
  :format '((profile bui-list-get-file-name 40 t)
            (number-of-packages nil 11 bui-list-sort-numerically-1
                                :right-align t)
            (number-of-generations nil 14 bui-list-sort-numerically-2
                                   :right-align t))
  :titles '((number-of-packages    . "Packages")
            (number-of-generations . "Generations"))
  :sort-key '(profile))

(let ((map guix-profile-list-mode-map))
  (define-key map [remap self-insert-command] 'guix-profile-list-hint)
  (define-key map (kbd "P") 'guix-profile-list-show-packages)
  (define-key map (kbd "G") 'guix-profile-list-show-generations)
  (define-key map (kbd "M") 'guix-profile-list-apply-manifest)
  (define-key map (kbd "c") 'guix-profile-list-set-current)
  ;; Unbind "i" and "RET" as "Profile Info" interface is not defined.
  (define-key map (kbd "i") nil)
  (define-key map (kbd "RET") 'guix-profile-list-hint))

(defun guix-profile-list-hint ()
  "Display a message with useful key bindings."
  (interactive)
  (message (substitute-command-keys "Hint:
Press '\\[guix-profile-list-show-packages]' to display packages.
Press '\\[guix-profile-list-show-generations]' to display generations.")))

(defun guix-profile-list-current-profile ()
  "Return file name of the current profile."
  ;; (bui-entry-value (bui-list-current-entry) 'profile)
  ;; Just get the ID, as currently ID is the profile file name.
  (bui-list-current-id))

(declare-function guix-installed-packages "guix-ui-package" t)
(declare-function guix-generations "guix-ui-generation" t)
(declare-function guix-system-generations "guix-ui-system-generation" t)
(declare-function guix-apply-manifest "guix-misc" t)

(defun guix-profile-list-show-packages ()
  "Display packages installed in the current profile."
  (interactive)
  (guix-installed-packages (guix-package-profile
                            (guix-profile-list-current-profile))))

(defun guix-profile-list-show-generations ()
  "Display generations of the current profile."
  (interactive)
  (let ((profile (guix-profile-list-current-profile)))
    (if (guix-system-profile? profile)
        (guix-system-generations)
      (guix-generations (guix-generation-profile profile)))))

(defun guix-profile-list-apply-manifest (file)
  "Apply manifest from FILE to the current profile."
  (interactive
   (list (read-file-name "File with manifest: ")))
  (guix-apply-manifest (guix-package-profile
                        (guix-profile-list-current-profile))
                       file (current-buffer)))

(defun guix-profile-list-set-current ()
  "Set `guix-current-profile' to the profile on the current line."
  (interactive)
  (guix-set-current-profile (guix-profile-list-current-profile)))


;;; Interactive commands

;;;###autoload
(defun guix-profiles ()
  "Display Guix profiles.
Modify `guix-profiles' variable to add more profiles."
  (interactive)
  (bui-list-get-display-entries 'guix-profile))

(provide 'guix-ui-profile)

;;; guix-ui-profile.el ends here
