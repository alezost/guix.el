;;; guix-list.el --- List buffers for displaying entries   -*- lexical-binding: t -*-

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

;; This file provides a list-like buffer for displaying information
;; about Guix packages and generations.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'guix-info)
(require 'guix-history)
(require 'guix-base)
(require 'guix-backend)
(require 'guix-utils)

(defgroup guix-list nil
  "General settings for list buffers."
  :prefix "guix-list-"
  :group 'guix)

(defface guix-list-file-path
  '((t :inherit guix-info-file-path))
  "Face used for file paths."
  :group 'guix-list)

(defcustom guix-list-describe-warning-count 10
  "The maximum number of entries for describing without a warning.
If a user wants to describe more than this number of marked
entries, he will be prompted for confirmation."
  :type 'integer
  :group 'guix-list)

(defvar guix-list-column-format
  '((package
     (name 20 t)
     (version 10 nil)
     (outputs 13 t)
     (installed 13 t)
     (synopsis 30 nil))
    (generation
     (number 5 t)
     (time 20 t)
     (path 30 t)))
  "Columns displayed in list buffers.
Each element of the list has a form:

  (ENTRY-TYPE . ((PARAM WIDTH SORT . PROPS) ...))

PARAM is the name of an entry parameter of ENTRY-TYPE.  For the
meaning of WIDTH, SORT and PROPS, see `tabulated-list-format'.")

(defvar guix-list-column-titles
  '((generation
     (number . "N.")))
  "Column titles for list buffers.
Has the same structure as `guix-param-titles', but titles from
this list have a priority.")

(defvar guix-list-column-value-methods
  '((package
     (name        . guix-package-list-get-name)
     (synopsis    . guix-list-get-one-line)
     (description . guix-list-get-one-line)
     (installed   . guix-package-list-get-installed-outputs))
    (generation
     (time . guix-list-get-time)
     (path . guix-list-get-file-path)))
  "Methods for inserting parameter values in columns.
Each element of the list has a form:

  (ENTRY-TYPE . ((PARAM . FUN) ...))

PARAM is the name of an entry parameter of ENTRY-TYPE.

FUN is a function returning a value that will be inserted.  The
function is called with 2 arguments: the first one is the value
of the parameter; the second argument is an entry info (alist of
parameters and their values).")

(defun guix-list-get-param-title (entry-type param)
  "Return title of an ENTRY-TYPE entry parameter PARAM."
  (or (guix-get-key-val guix-list-column-titles
                        entry-type param)
      (guix-get-param-title entry-type param)))

(defun guix-list-get-column-format (entry-type)
  "Return column format for ENTRY-TYPE."
  (guix-get-key-val guix-list-column-format entry-type))

(defun guix-list-get-displayed-params (entry-type)
  "Return list of parameters of ENTRY-TYPE that should be displayed."
  (mapcar #'car
          (guix-list-get-column-format entry-type)))

(defun guix-list-get-sort-key (entry-type param &optional revert)
  "Return suitable sort key for `tabulated-list-sort-key'.
Define column title by ENTRY-TYPE and PARAM.  If REVERT is
non-nil, revert the sort."
  (when (memq param (guix-list-get-displayed-params entry-type))
    (cons (guix-list-get-param-title entry-type param) revert)))

(defun guix-list-make-tabulated-vector (entry-type fun)
  "Call FUN on each column specification for ENTRY-TYPE.

FUN is called with 2 argument: parameter name and column
specification (see `guix-list-column-format').

Return a vector made of values of FUN calls."
  (apply #'vector
         (mapcar (lambda (col-spec)
                   (funcall fun (car col-spec) (cdr col-spec)))
                 (guix-list-get-column-format entry-type))))

(defun guix-list-get-list-format (entry-type)
  "Return ENTRY-TYPE list specification for `tabulated-list-format'."
  (guix-list-make-tabulated-vector
   entry-type
   (lambda (param spec)
     (cons (guix-list-get-param-title entry-type param)
           spec))))

(defun guix-list-insert-entries (entries entry-type)
  "Display ENTRIES of ENTRY-TYPE in the current list buffer.
ENTRIES should have a form of `guix-entries'."
  (setq tabulated-list-entries
        (guix-list-get-tabulated-entries entries entry-type))
  (tabulated-list-print))

(defun guix-list-get-tabulated-entries (entries entry-type)
  "Return list of values of ENTRY-TYPE for `tabulated-list-entries'.
Values are taken from ENTRIES which should have the form of
`guix-entries'."
  (mapcar (lambda (entry)
            (list (guix-get-key-val entry 'id)
                  (guix-list-get-tabulated-entry entry entry-type)))
          entries))

(defun guix-list-get-tabulated-entry (entry entry-type)
  "Return array of values for `tabulated-list-entries'.
Parameters are taken from ENTRY of ENTRY-TYPE."
  (guix-list-make-tabulated-vector
   entry-type
   (lambda (param _)
     (let ((val (guix-get-key-val entry param))
           (fun (guix-get-key-val guix-list-column-value-methods
                                  entry-type param)))
       (if (and val fun)
           (funcall fun val entry)
         (guix-get-string val))))))

(defun guix-list-get-one-line (str &optional _)
  "Return one-line string from a multi-line STR."
  (guix-get-one-line str))

(defun guix-list-get-time (seconds &optional _)
  "Return formatted time string from SECONDS."
  (guix-get-time-string seconds))

(defun guix-list-get-file-path (path &optional _)
  "Return PATH button specification for `tabulated-list-entries'."
  (list path
        'face 'guix-list-file-path
        'action (lambda (btn) (find-file (button-label btn)))
        'follow-link t
        'help-echo "Find file"))

(defun guix-list-current-id ()
  "Return ID of the current entry."
  (or (tabulated-list-get-id)
      (user-error "No entry here")))

(defun guix-list-for-each-line (fun &rest args)
  "Call FUN with ARGS for each entry line."
  (or (derived-mode-p 'guix-list-mode)
      (error "The current buffer is not in Guix List mode"))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply fun args)
      (forward-line))))

(defun guix-list-fold-lines (fun init)
  "Fold over entry lines in the current list buffer.
Call FUN with RESULT as argument for each line, using INIT as
the initial value of RESULT.  Return the final result."
  (let ((res init))
    (guix-list-for-each-line
     (lambda () (setq res (funcall fun res))))
    res))


;;; Marking

(defvar guix-list-mark-alist
  '((empty   . ?\s)
    (general . ?*))
  "Alist of available mark names and mark characters.")

(defsubst guix-list-get-mark (name)
  "Return mark character by its NAME."
  (or (guix-get-key-val guix-list-mark-alist name)
      (error "Mark '%S' not found" name)))

(defsubst guix-list-get-mark-string (name)
  "Return mark string by its NAME."
  (string (guix-list-get-mark name)))

(defun guix-list-current-mark ()
  "Return mark character of the current line."
  (char-after (line-beginning-position)))

(defun guix-list-get-marked-id-list (&rest mark-names)
  "Return list of IDs of the lines marked with any mark from MARK-NAMES.
If MARK-NAMES are not specified, use all marks from
`guix-list-mark-alist' except the `empty' one."
  (let ((marks (mapcar #'guix-list-get-mark
                       (or mark-names
                           (delq 'empty
                                 (mapcar #'car guix-list-mark-alist))))))
    (guix-list-fold-lines
     (lambda (ids)
       (if (memq (guix-list-current-mark) marks)
           (cons (guix-list-current-id) ids)
         ids))
     '())))

(defun guix-list-mark (name &optional advance)
  "Put a mark on the current line.
NAME is a mark name from `guix-list-mark-alist'.
If ADVANCE is non-nil, move forward by one line after marking.
Interactively, put a general mark and move to the next line."
  (interactive '(general t))
  (tabulated-list-put-tag (guix-list-get-mark-string name)
                          advance))

(defun guix-list-mark-all (name)
  "Mark all lines with NAME mark.
NAME is a mark name from `guix-list-mark-alist'.
Interactively, put a general mark on all lines."
  (interactive '(general))
  (guix-list-for-each-line #'guix-list-mark name))

(defun guix-list-unmark ()
  "Unmark the current line and move to the next line."
  (interactive)
  (guix-list-mark 'empty t))

(defun guix-list-unmark-backward ()
  "Move up one line and unmark it."
  (interactive)
  (forward-line -1)
  (guix-list-mark 'empty))

(defun guix-list-unmark-all ()
  "Unmark all lines."
  (interactive)
  (guix-list-mark-all 'empty))


(defvar guix-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "m")   'guix-list-mark)
    (define-key map (kbd "*")   'guix-list-mark)
    (define-key map (kbd "M")   'guix-list-mark-all)
    (define-key map (kbd "u")   'guix-list-unmark)
    (define-key map (kbd "U")   'guix-list-unmark-all)
    (define-key map (kbd "DEL") 'guix-list-unmark-backward)
    map)
  "Parent keymap for list buffers.")

(define-derived-mode guix-list-mode tabulated-list-mode "Guix-List"
  "Parent mode for displaying information in list buffers."
  (setq tabulated-list-padding 2))

(defmacro guix-list-define-entry-type (entry-type &rest args)
  "Define common stuff for displaying ENTRY-TYPE entries in list buffers.

Remaining argument (ARGS) should have a form [KEYWORD VALUE] ...  The
following keywords are available:

  - `:sort-key' - default sort key for the tabulated list buffer.

  - `:marks' - default value for the defined
    `guix-ENTRY-TYPE-mark-alist' variable.

This macro defines the following functions:

  - `guix-ENTRY-TYPE-describe' - display marked entries in info buffer.

  - `guix-ENTRY-TYPE-mark-MARK-NAME' functions for each mark
    specified in `:marks' argument."
  (let* ((entry-type-str (symbol-name entry-type))
         (entry-str      (concat entry-type-str " entries"))
         (prefix         (concat "guix-" entry-type-str "-list"))
         (mode-str       (concat prefix "-mode"))
         (init-fun       (intern (concat prefix "-mode-initialize")))
         (describe-fun   (intern (concat prefix "-describe")))
         (marks-var      (intern (concat prefix "-mark-alist")))
         (marks-val      nil)
         (sort-key       nil))

    ;; Process the keyword args.
    (while (keywordp (car args))
      (pcase (pop args)
        (`:sort-key (setq sort-key (pop args)))
	(`:marks    (setq marks-val (pop args)))
	(_ (pop args))))

    `(progn
       (defvar ,marks-var ',marks-val
         ,(concat "Alist of additional marks for `" mode-str "'.\n"
                  "Marks from this list are added to `guix-list-mark-alist'."))

       ,@(mapcar (lambda (mark-spec)
                   (let* ((mark-name (car mark-spec))
                          (mark-name-str (symbol-name mark-name)))
                     `(defun ,(intern (concat prefix "-mark-" mark-name-str)) ()
                        ,(concat "Put '" mark-name-str "' mark and move to the next line.")
                        (interactive)
                        (guix-list-mark ',mark-name t))))
                 marks-val)

       (defun ,describe-fun (&optional arg)
         ,(concat "Describe " entry-str " marked with a general mark.\n"
                  "If no entry is marked, describe the current " entry-type-str ".\n"
                  "With prefix (if ARG is non-nil), describe the " entry-str "\n"
                  "marked with any mark.")
         (interactive "P")
         (let* ((ids (or (apply #'guix-list-get-marked-id-list
                                (unless arg '(general)))
                         (list (guix-list-current-id))))
                (count (length ids)))
           (when (or (<= count guix-list-describe-warning-count)
                     (y-or-n-p (format "Do you really want to describe %d entries? "
                                       count)))
             (,(intern (concat "guix-" entry-type-str "-info-get-show"))
              'id ids))))

       (defun ,init-fun ()
         ,(concat "Initial settings for `" mode-str "'.")
         ,(when sort-key
            `(setq tabulated-list-sort-key (guix-list-get-sort-key
                                            ',entry-type ',sort-key)))
         (setq tabulated-list-format (guix-list-get-list-format ',entry-type))
         (setq-local guix-list-mark-alist
                     (append guix-list-mark-alist ,marks-var))
         (tabulated-list-init-header)))))

(put 'guix-list-define-entry-type 'lisp-indent-function 'defun)


;;; Displaying packages

(guix-define-buffer-type list package)

(guix-list-define-entry-type package
  :sort-key name
  :marks ((install . ?I)
          (delete  . ?D)))

(defface guix-package-list-obsolete
  '((t :inherit guix-package-info-obsolete))
  "Face used if a package is obsolete."
  :group 'guix-package-list)

(let ((map guix-package-list-mode-map))
  (define-key map (kbd "RET") 'guix-package-list-describe)
  (define-key map (kbd "i")   'guix-package-list-mark-install)
  (define-key map (kbd "d")   'guix-package-list-mark-delete))

(defun guix-package-list-get-name (name entry)
  "Return NAME of the package ENTRY.
Colorize it with `guix-package-list-obsolete' if needed."
  (guix-get-string name
                   (when (guix-get-key-val entry 'obsolete)
                     'guix-package-list-obsolete)))

(defun guix-package-list-get-installed-outputs (installed &optional _)
  "Return string with outputs from INSTALLED entries."
  (guix-get-string
   (mapcar (lambda (entry)
             (guix-get-key-val entry 'output))
           installed)))


;;; Displaying generations

(guix-define-buffer-type list generation)

(guix-list-define-entry-type generation
  :sort-key number
  :marks ((delete  . ?D)))

(let ((map guix-generation-list-mode-map))
  (define-key map (kbd "RET") 'guix-generation-list-describe)
  (define-key map (kbd "d")   'guix-generation-list-mark-delete))

(provide 'guix-list)

;;; guix-list.el ends here
