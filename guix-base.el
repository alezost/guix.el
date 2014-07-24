;;; guix-base.el --- Common definitions

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

;; This file provides some base and common definitions for guix.el
;; package.

;; List and info buffers have many common patterns that are defined
;; using `guix-define-buffer-type' macro from this file.

;;; Code:

(require 'cl-macs)
(require 'guix-backend)


;;; Parameters (fields) of the packages

(defvar guix-param-titles
  '((general
     (name              . "Name")
     (version           . "Version")
     (license           . "License")
     (synopsis          . "Synopsis")
     (description       . "Description")
     (home-url          . "Home page")
     (outputs           . "Outputs")
     (inputs            . "Inputs")
     (native-inputs     . "Native inputs")
     (propagated-inputs . "Propagated inputs")
     (location          . "Location")
     (installed         . "Installed"))
    (installed
     (path              . "Installed path")
     (dependencies      . "Dependencies")
     (output            . "Output")))
  "List for defining titles of package parameters.
Titles are used for displaying package information.
Each element of the list has a form:

  (INFO-TYPE (PARAM . TITLE) ...)")

(defun guix-get-param-title (param &optional info-type)
  "Return title of a package parameter PARAM of INFO-TYPE."
  (guix-get-string-by-key param
                          (guix-get-key-val (or info-type 'general)
                                            guix-param-titles)))

(defun guix-get-name-spec (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "-" version
          (when output (concat ":" output))))

(defun guix-get-full-name (info &optional output)
  "Return name specification of the package by its INFO and OUTPUT."
  (guix-get-name-spec (guix-get-key-val 'name info)
                      (guix-get-key-val 'version info)
                      output))


;;; Location of the packages

(defvar guix-directory nil
  "Default Guix directory.
If it is not set by a user, it is set after starting Guile REPL.
This directory is used to define location of the packages.")

(defun guix-set-directory ()
  "Set `guix-directory' if needed."
  (or guix-directory
      (setq guix-directory
            (guix-eval-read "guix-dir"))))

(add-hook 'guix-after-start-repl-hook 'guix-set-directory)

(defun guix-find-location (location)
  "Go to LOCATION of a package.
LOCATION is a string of the form:

  \"PATH:LINE:COLUMN\"

If PATH is relative, it is considered to be relative to
`guix-directory'."
  (cl-multiple-value-bind (path line col)
      (split-string location ":")
    (let ((file (expand-file-name path guix-directory))
          (line (string-to-number line))
          (col  (string-to-number col)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (- line 1))
      (move-to-column col)
      (recenter 1))))


;;; Common definitions for buffer types

(defvar-local guix-packages nil
  "List of the currently displayed packages.
Each element of the list is alist with package info of the
following form:

  ((PARAM . VAL) ...)

PARAM is a name of the package parameter.
VAL is a value of this parameter.")
(put 'guix-packages 'permanent-local t)

(defvar-local guix-search-type nil
  "Type of the current search.
Function `guix-get-packages' called with `guix-search-type' and
`guix-search-vals' returned the current `guix-packages'.")
(put 'guix-search-type 'permanent-local t)

(defvar-local guix-search-vals nil
  "Values of the current search.
Function `guix-get-packages' called with `guix-search-type' and
`guix-search-vals' returned the current `guix-packages'.")
(put 'guix-search-vals 'permanent-local t)

(defsubst guix-set-vars (packages search-type search-vals)
  (setq guix-packages packages
        guix-search-vals search-vals
        guix-search-type search-type))

(defmacro guix-define-buffer-type (type parent-mode)
  "Define common stuff for TYPE of buffer for displaying packages.
PARENT-MODE is a parent mode for the `guix-TYPE-mode'."
  (let ((type-str (symbol-name type)))
    (let ((group         (intern (concat "guix-" type-str)))
          (mode          (intern (concat "guix-" type-str "-mode")))
          (mode-map      (intern (concat "guix-" type-str "-mode-map")))
          (mode-init-fun (intern (concat "guix-" type-str "-mode-initialize")))
          (buf-name-var  (intern (concat "guix-" type-str "-buffer-name")))
          (mode-name-var (intern (concat "guix-" type-str "-mode-name")))
          (revert-var    (intern (concat "guix-" type-str "-revert-no-confirm")))
          (revert-fun    (intern (concat "guix-" type-str "-revert")))
          (redisplay-fun (intern (concat "guix-" type-str "-redisplay")))
          (history-var   (intern (concat "guix-" type-str "-history-size")))
          (history-fun   (intern (concat "guix-" type-str "-make-history-item")))
          (get-fun       (intern (concat "guix-" type-str "-get-packages")))
          (set-fun       (intern (concat "guix-" type-str "-set")))
          (show-fun      (intern (concat "guix-" type-str "-show-packages")))
          (get-show-fun  (intern (concat "guix-" type-str "-get-show-packages")))
          (insert-fun    (intern (concat "guix-" type-str "-insert-packages"))))
      `(progn
         (defgroup ,group nil
           ,(concat "Buffer with Guix packages.")
           :prefix ,(concat "guix-" type-str "-")
           :group 'guix)

         (defcustom ,buf-name-var ,(concat "*Guix Package " type-str "*")
           ,(concat "Default name of the " type-str " buffer.")
           :type 'string
           :group ',group)

         (defcustom ,history-var 30
           ,(concat "Maximum number of items saved in the history of the " type-str " buffer.\n"
                    "If 0, the history is disabled.")
           :type 'integer
           :group ',group)

         (defcustom ,revert-var nil
           ,(concat "If non-nil, do not ask to confirm for reverting the " type-str " buffer.")
           :type 'boolean
           :group ',group)

         (define-derived-mode ,mode ,parent-mode ,(concat "Guix-" type-str)
           ,(concat "Major mode for displaying information about Guix packages.\n\n"
                    "\\{guix-" type-str "-mode-map}")
           (setq-local revert-buffer-function ',revert-fun)
           (setq-local guix-history-size ,history-var)
           (and (fboundp ',mode-init-fun) (,mode-init-fun)))

         (defun ,revert-fun (_ignore-auto noconfirm)
           "Update information in the current buffer.
The function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
           (when (or ,revert-var
                     noconfirm
                     (y-or-n-p "Update current information? "))
             (let ((packages (,get-fun guix-search-type guix-search-vals)))
               (,set-fun packages guix-search-type guix-search-vals t))))

         (defun ,redisplay-fun ()
           "Redisplay current information.
This function will not update the information, use
\"\\[revert-buffer]\" if you want the full update."
           (interactive)
           (,show-fun guix-packages))

         (defun ,history-fun ()
           "Make and return a history item for the current buffer."
           (list (lambda (packages search-type search-vals)
                   (,show-fun packages)
                   (guix-set-vars packages search-type search-vals))
                 guix-packages guix-search-type guix-search-vals))

         (defun ,set-fun (packages search-type search-vals &optional history-replace)
           ,(concat "Set up the " type-str " buffer.\n"
                    "Display PACKAGES, set variables and make history item.\n\n"
                    "PACKAGES should have a form of `guix-packages'.\n\n"
                    "See `guix-get-packages' for the meaning of SEARCH-TYPE and\n"
                    "SEARCH-VALS.\n\n"
                    "If HISTORY-REPLACE is non-nil, replace current history item,\n"
                    "otherwise add the new one.")
           (let ((buf (if (eq major-mode ',mode)
                          (current-buffer)
                        (get-buffer-create ,buf-name-var))))
             (with-current-buffer buf
               (,show-fun packages)
               (guix-set-vars packages search-type search-vals)
               (funcall (if history-replace
                            #'guix-history-replace
                          #'guix-history-add)
                        (,history-fun)))
             (pop-to-buffer-same-window buf)))

         (defun ,show-fun (packages)
           "Display PACKAGES in the current buffer."
           (let ((inhibit-read-only t))
             (erase-buffer)
             (,mode)
             (,insert-fun packages)
             (goto-char (point-min)))
           (when (cdr packages)
             (message "%d packages." (length packages))))

         (defun ,get-show-fun (search-type &rest search-vals)
           ,(concat "Search for packages and show results in the " type-str " buffer.\n"
                    "See `guix-get-packages' for the meaning of SEARCH-TYPE and\n"
                    "SEARCH-VALS.")
           (let ((packages (,get-fun search-type search-vals)))
             (,set-fun packages search-type search-vals)))

         (let ((map ,mode-map))
           (define-key map (kbd "l") 'guix-history-back)
           (define-key map (kbd "r") 'guix-history-forward)
           (define-key map (kbd "R") ',redisplay-fun)
           (define-key map (kbd "C-c C-z") 'guix-switch-to-repl))))))

(provide 'guix-base)

;;; guix-base.el ends here
