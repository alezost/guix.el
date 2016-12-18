;;; guix-about.el --- Various info about Guix and Emacs-Guix

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

;; This file provides the code to display various info about Guix (e.g., its
;; version).

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-utils)
(require 'guix-config)

(declare-function guix-eval-read "guix-repl" (str))

;;;###autoload
(defun guix-version ()
  "Display Emacs-Guix and Guix versions in the echo area."
  (interactive)
  (require 'guix-repl)
  (message "%s %s\n%s %s"
           (guix-eval-read "(@ (guix config) %guix-package-name)")
           (guix-eval-read "(@ (guix config) %guix-version)")
           guix-config-name
           guix-config-version))


;;; "Guix Help" buffer

(guix-define-groups help
  :group-doc "Settings for '\\[guix-help]'."
  :faces-group-doc "Faces for '\\[guix-help]'.")

(defcustom guix-help-buffer-name "*Guix Help*"
  "Buffer name for '\\[guix-help]'."
  :type 'string
  :group 'guix-help)

(defcustom guix-help-doc-column 40
  "Column at which 'doc' button is inserted."
  :type 'integer
  :group 'guix-help)

(defface guix-help-heading
  '((t :inherit bui-info-heading))
  "Face for headings in `guix-help-buffer-name' buffer."
  :group 'guix-help-faces)

(defvar guix-help-specifications
  '("Show packages"
    guix-all-available-packages
    guix-newest-available-packages
    guix-installed-user-packages
    guix-installed-system-packages
    guix-installed-packages
    guix-obsolete-packages
    guix-packages-by-name
    guix-packages-by-license
    guix-packages-by-location
    guix-package-from-file
    guix-search-by-name
    guix-search-by-regexp

    "Show profiles"
    guix-profiles

    "Show profile generations"
    guix-generations
    guix-last-generations
    guix-generations-by-time
    guix-system-generations
    guix-last-system-generations
    guix-system-generations-by-time

    "Show/browse package licenses"
    guix-licenses
    guix-browse-license-url
    guix-find-license-definition

    "Show/find package locations"
    guix-locations
    guix-find-location
    guix-edit

    "Magit-like interface"
    guix

    "Show Hydra builds and jobsets"
    guix-hydra-latest-builds
    guix-hydra-queued-builds
    guix-hydra-jobsets

    "Hide hash parts in \"/gnu/store/…-foo\" file names"

    (guix-prettify-mode nil)
    global-guix-prettify-mode

    "Highlighting for package build logs"
    (guix-build-log-mode nil)
    (guix-build-log-minor-mode nil)

    "Highlighting for Guix .scm files"
    (guix-devel-mode nil)

    "Miscellaneous commands"
    guix-emacs-autoload-packages ; available in Emacs installed with Guix
    guix-set-current-profile
    guix-pull
    guix-apply-manifest
    guix-version)
  "List of command specifications for '\\[guix-help]'.
Each specification can have one of the following forms:

  TITLE
  COMMAND-NAME
  (COMMAND-NAME COMMAND-BUTTON?)

TITLE is a string; COMMAND-NAME is a symbol; COMMAND-BUTTON? is a
boolean value, that defines whether COMMAND-NAME is buttonized or
not.")

(defun guix-insert-info-button (label info-node)
  "Insert button with LABEL to open texinfo manual.
INFO-NODE is the name passed to `info' function."
  (bui-insert-button
   label 'button
   'action (lambda (button)
             (info (button-get button 'node)))
   'node info-node))

(defun guix-insert-doc-button (label symbol)
  "Insert button with LABEL to open the docstring of SYMBOL."
  (bui-insert-button
   label 'button
   'help-echo (format "Display documentation of '%S'" symbol)
   'action (lambda (button)
             (describe-symbol (button-get button 'symbol)))
   'symbol symbol))

(defun guix-insert-command-button (command)
  "Insert button to run 'M-x COMMAND'."
  (let ((command-string (symbol-name command)))
    (bui-insert-button
     command-string 'button
     'help-echo (format "Call 'M-x %s'" command-string)
     'action (lambda (button)
               (call-interactively (button-get button 'command)))
     'command command)))

(defun guix-help-insert-specification (spec)
  "Insert command specification SPEC at point.
See `guix-help-specifications' for the meaning of SPEC."
  (pcase spec
    ((pred symbolp)
     (guix-help-insert-specification (list spec t)))
    ((pred stringp)
     (bui-newline)
     (bui-format-insert spec 'guix-help-heading)
     (bui-newline 2))
    (`(,name ,command-button?)
     (when (fboundp name)
       (bui-with-indent bui-indent
         (if command-button?
             (guix-insert-command-button name)
           (insert (symbol-name name)))
         (indent-to guix-help-doc-column 2)
         (guix-insert-doc-button "doc" name))
       (bui-newline)))
    (_
     (insert "<unknown specification>")
     (bui-newline))))

(defun guix-help-revert (_ignore-auto noconfirm)
  "Revert function for `revert-buffer-function'."
  (when (or noconfirm
            (y-or-n-p (format "Revert %s buffer? " (buffer-name))))
    (guix-help-insert-content)))

(defun guix-help-insert-content ()
  "Insert Emacs-Guix help info into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (guix-insert-info-button "GNU Guix Manual" "guix")
    (bui-newline)
    (guix-insert-info-button "Emacs-Guix Manual" "emacs-guix")
    (bui-newline 2)
    (insert "Summary of the available M-x commands:\n")
    (mapc #'guix-help-insert-specification
          guix-help-specifications)))

;;;###autoload
(defun guix-help ()
  "Display a summary of the available Emacs-Guix commands."
  (interactive)
  (with-output-to-temp-buffer guix-help-buffer-name
    (set-buffer guix-help-buffer-name)
    (setq-local revert-buffer-function 'guix-help-revert)
    (guix-help-insert-content)))

(provide 'guix-about)

;;; guix-about.el ends here
