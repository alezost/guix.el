;;; guix-info.el --- Major mode for displaying Guix packages

;; Copyright © 2014 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a help-like buffer for displaying information
;; about Guix packages.

;;; Code:

(require 'guix-history)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-utils)

(defvar guix-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-buffer-map
                               special-mode-map))
    map)
  "Keymap for `guix-info-mode'.")

(guix-define-buffer-type info special-mode)

(defface guix-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for a title of a package parameter."
  :group 'guix-info)

(defface guix-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'guix-info)

(defface guix-info-version
  '((t :inherit font-lock-builtin-face))
  "Face used for a version of a package."
  :group 'guix-info)

(defface guix-info-synopsis
  '((t :inherit font-lock-doc-face))
  "Face used for a synopsis of a package."
  :group 'guix-info)

(defface guix-info-description
  '((t))
  "Face used for a description of a package."
  :group 'guix-info)

(defface guix-info-license
  '((t :inherit font-lock-string-face))
  "Face used for a license of a package."
  :group 'guix-info)

(defface guix-info-file-path
  '((t :inherit button))
  "Face used for file paths."
  :group 'guix-info)

(defface guix-info-location
  '((t :inherit button))
  "Face used for a location of a package."
  :group 'guix-info)

(defface guix-info-url
  '((t :inherit button))
  "Face used for URLs."
  :group 'guix-info)

(defface guix-info-inputs
  '((t :inherit button))
  "Face used for inputs of a package."
  :group 'guix-info)

(defface guix-info-native-inputs
  '((t :inherit guix-info-inputs))
  "Face used for native inputs of a package."
  :group 'guix-info)

(defface guix-info-installed-outputs
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Face used for installed outputs of a package."
  :group 'guix-info)

(defface guix-info-uninstalled-outputs
  '((t :weight bold))
  "Face used for uninstalled outputs of a package."
  :group 'guix-info)

(defface guix-info-obsolete
  '((t :inherit error))
  "Face used if a package is obsolete."
  :group 'guix-info)

(defface guix-info-action-button
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black")
    (t :inherit button))
  "Face used for \"Insert\"/\"Delete\"/\"Update\" buttons."
  :group 'guix-info)

(defface guix-info-action-button-mouse
  '((((type x w32 ns) (class color))
     :box (:line-width 2 :style released-button)
     :background "grey90" :foreground "black")
    (t :inherit highlight))
  "Mouse face used for \"Insert\"/\"Delete\"/\"Update\" buttons."
  :group 'guix-info)

(defcustom guix-info-ignore-empty-vals nil
  "If non-nil, do not display parameters with nil values."
  :type 'boolean
  :group 'guix-info)

(defvar guix-info-param-title-format "%-14s: "
  "String used to format a title of a package parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.")

(defvar guix-info-multiline-prefix (make-string 16 ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.")

(defvar guix-info-fill-column 60
  "Column used for filling (word wrapping) parameters with long lines.
If a value is not multi-line and it occupies more than this
number of characters, it will be split into several lines.")

(defvar guix-info-delimiter "\n\f\n"
  "String used to separate packages info.")

(defvar guix-info-insert-methods
  '((general
     (name          guix-info-name)
     (version       guix-info-version)
     (license       guix-info-license)
     (synopsis      guix-info-synopsis)
     (description   guix-info-description)
     (outputs       guix-info-insert-outputs
                    guix-info-insert-title-simple)
     (home-url      guix-info-insert-url)
     (inputs        guix-info-insert-inputs)
     (native-inputs guix-info-insert-native-inputs)
     (location      guix-info-insert-location))
    (installed
     (path          guix-info-insert-output-path
                    guix-info-insert-title-installed)
     (dependencies  guix-info-insert-output-dependencies
                    guix-info-insert-title-installed)))
  "List of methods for inserting values of package parameters.
Each element of the list should have a form:

  (INFO-TYPE (PARAM INSERT-VALUE [INSERT-TITLE]) ...)

INSERT-VALUE may be either nil, a face name or a function.  If it
is nil or a face, `guix-info-insert-val-default' function is
called with parameter value and INSERT-VALUE as arguments.  If it
is a function, this function is called with parameter value and a
package info (alist of parameters and their values) as arguments.

INSERT-TITLE may be either nil, a face name or a function.  If it
is nil or a face, `guix-info-insert-title-default' function is
called with parameter title and INSERT-TITLE as arguments.  If it
is a function, this function is called with parameter title as
argument.")

(defvar guix-info-displayed-params
  '((general name version synopsis outputs location home-url
             license inputs native-inputs description)
    (installed path dependencies))
  "List of parameters displayed in the info buffer.
Each element of the list should have a form:

  (INFO-TYPE PARAM ...)

The order of displayed parameters is the same as in this list.")

(defalias 'guix-info-get-packages 'guix-get-packages)

(defun guix-info-get-insert-methods (param &optional info-type)
  "Return list of insert methods for parameter PARAM of INFO-TYPE.
See `guix-info-insert-methods' for details."
  (guix-get-key-val param
                    (guix-get-key-val (or info-type 'general)
                                      guix-info-insert-methods)))

(defun guix-info-get-displayed-params (&optional info-type)
  "Return parameters of INFO-TYPE that should be displayed."
  (guix-get-key-val (or info-type 'general)
                    guix-info-displayed-params))

(defun guix-info-insert-packages (packages)
  "Display PACKAGES in the current info buffer.
PACKAGES should have a form of `guix-packages'."
  (guix-mapinsert (lambda (info)
                    (guix-info-insert-info info))
                  packages
                  guix-info-delimiter))

(defun guix-info-insert-info (info &optional info-type)
  "Insert package INFO of INFO-TYPE into the current buffer."
  (mapc (lambda (param)
          (guix-info-insert-param param info info-type))
        (guix-info-get-displayed-params info-type)))

(defun guix-info-insert-param (param info &optional info-type)
  "Insert title and value of a parameter PARAM of INFO-TYPE at point.
INFO is alist with parameters and their values."
  (let ((val (guix-get-key-val param info)))
    (unless (and guix-info-ignore-empty-vals (null val))
      (let* ((title          (guix-get-param-title param info-type))
             (insert-methods (guix-info-get-insert-methods param info-type))
             (val-method     (car insert-methods))
             (title-method   (cadr insert-methods)))
        (guix-info-method-funcall title title-method
                                  #'guix-info-insert-title-default)
        (guix-info-method-funcall val val-method
                                  #'guix-info-insert-val-default
                                  info)
        (insert "\n")))))

(defun guix-info-method-funcall (val method default-fun &rest args)
  "Call METHOD or DEFAULT-FUN.

If FACE-OR-FUN is a function and VAL is non-nil, call this
function by applying it to VAL and ARGS.

If FACE-OR-FUN is a face, propertize inserted VAL with this face."
  (cond ((or (null method)
             (facep method))
         (funcall default-fun val method))
        ((functionp method)
         (apply method val args))
        (t (error "Unknown method '%S'" method))))

(defun guix-info-insert-title-default (title &optional face format)
  "Insert TITLE formatted with `guix-info-param-title-format' at point."
  (guix-format-insert title
                      (or face 'guix-info-param-title)
                      (or format guix-info-param-title-format)))

(defun guix-info-insert-title-simple (title &optional face)
  "Insert TITLE at point."
  (guix-info-insert-title-default title face "%s:"))

(defun guix-info-insert-val-default (val &optional face)
  "Format and insert parameter value VAL at point.

If VAL is a one-line string longer than `guix-info-fill-column',
split it into several short lines.

If FACE is non-nil, propertize inserted line(s) with this FACE."
  (if (stringp val)
      (let ((strings (split-string val "\n *")))
        (and (null (cdr strings))       ; if not multi-line
             (> (length val) guix-info-fill-column)
             (setq strings
                   (split-string (guix-get-filled-string
                                  val guix-info-fill-column)
                                 "\n")))
        (guix-mapinsert (lambda (str)
                          (guix-format-insert str face))
                        strings
                        (concat "\n" guix-info-multiline-prefix)))
    (guix-format-insert val face)))

(defun guix-info-insert-file-path (path _)
  "Make button from file PATH and insert it at point."
  (guix-insert-button
   path 'guix-info-file-path
   (lambda (btn) (find-file (button-label btn)))
   "Find file"))

(defun guix-info-insert-location (location _)
  "Make button from file LOCATION and insert it at point."
  (guix-insert-button
   location 'guix-info-location
   (lambda (btn) (guix-find-location (button-label btn)))
   "Find location of this package"))

(defun guix-info-insert-url (url _)
  "Make button from URL and insert it at point."
  (guix-insert-button
   url 'guix-info-url
   (lambda (btn) (browse-url (button-label btn)))
   "Browse URL"))

(defun guix-info-insert-inputs (inputs _)
  "Make buttons from INPUTS and insert those at point."
  (guix-info-insert-package-names inputs 'guix-info-inputs))

(defun guix-info-insert-native-inputs (inputs _)
  "Make buttons from native INPUTS and insert those at point."
  (guix-info-insert-package-names inputs 'guix-info-native-inputs))

(defun guix-info-insert-package-names (names face)
  "Make buttons from package NAMES and insert those at point.
NAMES is a list of strings.
Propertize buttons with FACE."
  (if names
      (guix-info-insert-val-default
       (with-temp-buffer
         (guix-mapinsert (lambda (name)
                           (guix-info-insert-package-name name face))
                         names
                         guix-list-separator)
         (buffer-substring (point-min) (point-max))))
    (guix-format-insert nil)))

(defun guix-info-insert-package-name (name face)
  "Make button and insert package NAME at point.
Propertize package button with FACE."
  (guix-insert-button
   name face
   (lambda (btn) (guix-info-get-show-packages 'name (button-label btn)))
   "Describe the package"))


;;; Inserting outputs and installed params

(defvar guix-info-output-format "%-10s"
  "String used to format output names of the packages.
It should be a '%s'-sequence.  After inserting an output name
formatted with this string, an action button is inserted.")

(defvar guix-info-output-indent-string (make-string 2 ?\s)
  "String used to indent package outputs.
This string is used to indent/separate various parts during
inserting outputs and installed info.")

(defvar guix-info-obsolete-string "(This package is obsolete)"
  "String used if a package is obsolete.")

(defun guix-info-output-indent (&optional level)
  "Insert `guix-info-output-indent-string' LEVEL times (1 by default)."
  (dotimes (i (or level 1))
    (insert guix-info-output-indent-string)))

(defun guix-info-insert-outputs (outputs info)
  "Insert OUTPUTS from INFO at point."
  (let ((obsolete  (guix-get-key-val 'obsolete info)))
    (when obsolete
      (guix-info-output-indent)
      (guix-format-insert guix-info-obsolete-string
                          'guix-info-obsolete)))
  (insert "\n")
  (mapc (lambda (output)
          (guix-info-insert-output output info))
        outputs))

(defun guix-info-insert-output (output info)
  "Insert OUTPUT at point.
Make some fancy text with buttons and additional stuff if the
current OUTPUT is installed (if there is such output in
`installed' parameter of a package INFO)."
  (let* ((installed (guix-get-key-val 'installed info))
         (obsolete  (guix-get-key-val 'obsolete info))
         (full-name (guix-get-full-name info output))
         (installed-info (cl-find-if
                          (lambda (info)
                            (string= (guix-get-key-val 'output info)
                                     output))
                          installed))
         (action-type (if installed-info 'delete 'install)))
    (guix-info-output-indent)
    (guix-format-insert output
                        (if installed-info
                            'guix-info-installed-outputs
                          'guix-info-uninstalled-outputs)
                        guix-info-output-format)
    (guix-info-insert-action-button action-type full-name)
    (when obsolete
      (guix-info-output-indent)
      (guix-info-insert-action-button 'update full-name))
    (insert "\n")
    (when installed-info
      (guix-info-insert-info installed-info 'installed))))

(defun guix-info-insert-action-button (type name)
  "Insert action button at point.
TYPE is one of the following symbols: `install', `delete', `update'.
NAME is a full name specification of the package."
  (let ((type-str (capitalize (symbol-name type))))
    (guix-insert-button
     type-str 'guix-info-action-button
     (lambda (btn) (error "Sorry, not implemented yet"))
     (concat type-str " '" name "'")
     'mouse-face 'guix-info-action-button-mouse)))

(defun guix-info-insert-title-installed (title &optional face)
  "Insert TITLE of the installed package parameter at point."
  (guix-info-output-indent 2)
  (guix-info-insert-title-simple title face))

(defun guix-info-insert-output-path (path _)
  "Insert PATH of the installed output."
  (insert "\n")
  (guix-info-output-indent 3)
  (guix-info-insert-file-path path nil))

(defun guix-info-insert-output-dependencies (deps _)
  "Insert dependencies DEPS of the installed output."
  (if deps
      (mapc (lambda (dep)
              (guix-info-insert-output-path (cadr dep) nil))
            deps)
    (guix-info-output-indent)
    (guix-format-insert nil)))

(provide 'guix-info)

;;; guix-info.el ends here
