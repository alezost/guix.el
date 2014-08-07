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

(require 'cl-lib)
(require 'guix-backend)


;;; Parameters of the entries

(defvar guix-param-titles
  '((package
     (id                . "ID")
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
     (output            . "Output"))
    (generation
     (id                . "ID")
     (number            . "Number")
     (prev-number       . "Previous number")
     (path              . "Path")
     (time              . "Time")))
  "List for defining titles of entry parameters.
Titles are used for displaying information about entries.
Each element of the list has a form:

  (ENTRY-TYPE . ((PARAM . TITLE) ...))")

(defun guix-get-param-title (entry-type param)
  "Return title of an ENTRY-TYPE entry parameter PARAM."
  (or (guix-get-key-val guix-param-titles
                        entry-type param)
      (prog1 (symbol-name param)
        (message "Couldn't find title for '%S %S'."
                 entry-type param))))

(defun guix-get-name-spec (name version &optional output)
  "Return Guix package specification by its NAME, VERSION and OUTPUT."
  (concat name "-" version
          (when output (concat ":" output))))

(defun guix-get-full-name (entry &optional output)
  "Return name specification of the package ENTRY and OUTPUT."
  (guix-get-name-spec (guix-get-key-val entry 'name)
                      (guix-get-key-val entry 'version)
                      output))

(defun guix-get-installed-outputs (entry)
  "Return list of installed outputs for the package ENTRY."
  (mapcar (lambda (installed-entry)
            (guix-get-key-val installed-entry 'output))
          (guix-get-key-val entry 'installed)))

(defun guix-get-entry-by-id (id entries)
  "Return entry from ENTRIES by entry ID."
  (cl-find-if (lambda (entry)
                (equal id (guix-get-key-val entry 'id)))
              entries))


;;; Location of the packages

(defvar guix-directory nil
  "Default Guix directory.
If it is not set by a user, it is set after starting Guile REPL.
This directory is used to define location of the packages.")

(defun guix-set-directory ()
  "Set `guix-directory' if needed."
  (or guix-directory
      (setq guix-directory
            (guix-eval-read "%guix-dir"))))

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

(defvar-local guix-entries nil
  "List of the currently displayed entries.
Each element of the list is alist with entry info of the
following form:

  ((PARAM . VAL) ...)

PARAM is a name of the entry parameter.
VAL is a value of this parameter.")
(put 'guix-entries 'permanent-local t)

(defvar-local guix-search-type nil
  "Type of the current search.")
(put 'guix-search-type 'permanent-local t)

(defvar-local guix-search-vals nil
  "Values of the current search.")
(put 'guix-search-vals 'permanent-local t)

(defsubst guix-set-vars (entries search-type search-vals)
  (setq guix-entries entries
        guix-search-type search-type
        guix-search-vals search-vals))

(defmacro guix-define-buffer-type (buf-type entry-type &rest args)
  "Define common stuff for BUF-TYPE buffers for displaying entries.

ENTRY-TYPE is a type of displayed entries (see
`guix-search-entries-config').

In the text below TYPE means ENTRY-TYPE-BUF-TYPE.

This macro defines `guix-TYPE-mode', a custom group, several user
variables and the following functions:

  - `guix-TYPE-get-params-for-receiving'
  - `guix-TYPE-revert'
  - `guix-TYPE-redisplay'
  - `guix-TYPE-make-history-item'
  - `guix-TYPE-set'
  - `guix-TYPE-show'
  - `guix-TYPE-get-show'

The following stuff should be defined outside this macro:

  - `guix-BUF-TYPE-mode' - parent mode for the defined mode.

  - `guix-BUF-TYPE-insert-entries' - function for inserting
  entries in the current buffer; it is called with 2 arguments:
  entries of the form of `guix-entries' and ENTRY-TYPE.

  - `guix-BUF-TYPE-get-displayed-params' - function returning a
  list of parameters displayed in the current buffer; it is
  called with ENTRY-TYPE as argument.

  - `guix-TYPE-mode-initialize' (optional) - function for
  additional mode settings; it is called without arguments.

Remaining argument (ARGS) should have a form [KEYWORD VALUE] ...  The
following keywords are available:

  - `:required' - default value for the defined
    `guix-TYPE-required-params' variable.

  - `:history-size' - default value for the defined
    `guix-TYPE-history-size' variable.

  - `:revert' - default value for the defined
    `guix-TYPE-revert-no-confirm' variable."
  (let* ((entry-type-str (symbol-name entry-type))
         (buf-type-str   (symbol-name buf-type))
         (Entry-type-str (capitalize entry-type-str))
         (Buf-type-str   (capitalize buf-type-str))
         (entry-str      (concat entry-type-str " entries"))
         (buf-str        (concat buf-type-str " buffer"))
         (prefix         (concat "guix-" entry-type-str "-" buf-type-str))
         (group          (intern prefix))
         (mode-map-str   (concat prefix "-mode-map"))
         (mode-map       (intern mode-map-str))
         (parent-mode    (intern (concat "guix-" buf-type-str "-mode")))
         (mode           (intern (concat prefix "-mode")))
         (mode-init-fun  (intern (concat prefix "-mode-initialize")))
         (buf-name-var   (intern (concat prefix "-buffer-name")))
         (mode-name-var  (intern (concat prefix "-mode-name")))
         (revert-var     (intern (concat prefix "-revert-no-confirm")))
         (revert-fun     (intern (concat prefix "-revert")))
         (redisplay-fun  (intern (concat prefix "-redisplay")))
         (history-var    (intern (concat prefix "-history-size")))
         (history-fun    (intern (concat prefix "-make-history-item")))
         (params-var     (intern (concat prefix "-required-params")))
         (params-fun     (intern (concat prefix "-get-params-for-receiving")))
         (set-fun        (intern (concat prefix "-set")))
         (show-fun       (intern (concat prefix "-show")))
         (get-show-fun   (intern (concat prefix "-get-show")))
         (insert-fun     (intern (concat prefix "-insert")))
         (revert-val     nil)
         (history-val    20)
         (params-val     '(id)))

    ;; Process the keyword args.
    (while (keywordp (car args))
      (pcase (pop args)
	(`:required     (setq params-val (pop args)))
	(`:history-size (setq history-val (pop args)))
	(`:revert       (setq revert-val (pop args)))
	(_ (pop args))))

    `(progn
       (defgroup ,group nil
         ,(concat Buf-type-str " buffer with " entry-str ".")
         :prefix ,(concat prefix "-")
         :group ',(intern (concat "guix-" buf-type-str)))

       (defcustom ,buf-name-var ,(format "*Guix %s %s*"
                                         Entry-type-str Buf-type-str)
         ,(concat "Default name of the " buf-str " for displaying " entry-str ".")
         :type 'string
         :group ',group)

       (defcustom ,history-var ,history-val
         ,(concat "Maximum number of items saved in the history of the " buf-str ".\n"
                  "If 0, the history is disabled.")
         :type 'integer
         :group ',group)

       (defcustom ,revert-var ,revert-val
         ,(concat "If non-nil, do not ask to confirm for reverting the " buf-str ".")
         :type 'boolean
         :group ',group)

       (defvar ,params-var ',params-val
         ,(concat "List of required " entry-type-str " parameters.\n\n"
                  "Displayed parameters and parameters from this list are received\n"
                  "for each " entry-type-str ".\n\n"
                  "May be a special value `all', in which case all supported\n"
                  "parameters are received (this may be very slow for a big number\n"
                  "of entries).\n\n"
                  "Do not remove `id' from this list as it is required for\n"
                  "identifying an entry."))

       (define-derived-mode ,mode ,parent-mode ,(concat "Guix-" Buf-type-str)
         ,(concat "Major mode for displaying information about " entry-str ".\n\n"
                  "\\{" mode-map-str "}")
         (setq-local revert-buffer-function ',revert-fun)
         (setq-local guix-history-size ,history-var)
         (and (fboundp ',mode-init-fun) (,mode-init-fun)))

       (let ((map ,mode-map))
         (define-key map (kbd "l") 'guix-history-back)
         (define-key map (kbd "r") 'guix-history-forward)
         (define-key map (kbd "g") 'revert-buffer)
         (define-key map (kbd "R") ',redisplay-fun)
         (define-key map (kbd "C-c C-z") 'guix-switch-to-repl))

       (defun ,params-fun ()
         ,(concat "Return " entry-type-str " parameters that should be received.")
         (unless (equal ,params-var 'all)
           (cl-union ,params-var
                     (,(intern (concat "guix-" buf-type-str "-get-displayed-params"))
                      ',entry-type))))

       (defun ,revert-fun (_ignore-auto noconfirm)
         "Update information in the current buffer.
The function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
         (when (or ,revert-var
                   noconfirm
                   (y-or-n-p "Update current information? "))
           (let ((entries (guix-get-entries ',entry-type guix-search-type
                                            guix-search-vals (,params-fun))))
             (,set-fun entries guix-search-type guix-search-vals t))))

       (defun ,redisplay-fun ()
         "Redisplay current information.
This function will not update the information, use
\"\\[revert-buffer]\" if you want the full update."
         (interactive)
         (,show-fun guix-entries)
         (guix-result-message guix-entries ',entry-type
                              guix-search-type guix-search-vals))

       (defun ,history-fun ()
         "Make and return a history item for the current buffer."
         (list (lambda (entries search-type search-vals)
                 (,show-fun entries)
                 (guix-set-vars entries search-type search-vals)
                 (guix-result-message entries ',entry-type
                                      search-type search-vals))
               guix-entries guix-search-type guix-search-vals))

       (defun ,set-fun (entries search-type search-vals &optional history-replace)
         ,(concat "Set up the " buf-str " for displaying " entry-str ".\n\n"
                  "Display ENTRIES, set variables and make history item.\n\n"
                  "ENTRIES should have a form of `guix-entries'.\n\n"
                  "See `guix-get-entries' for the meaning of SEARCH-TYPE and\n"
                  "SEARCH-VALS.\n\n"
                  "If HISTORY-REPLACE is non-nil, replace current history item,\n"
                  "otherwise add the new one.")
         (when entries
           (let ((buf (if (eq major-mode ',mode)
                          (current-buffer)
                        (get-buffer-create ,buf-name-var))))
             (with-current-buffer buf
               (,show-fun entries)
               (guix-set-vars entries search-type search-vals)
               (funcall (if history-replace
                            #'guix-history-replace
                          #'guix-history-add)
                        (,history-fun)))
             (pop-to-buffer buf
                            '((display-buffer-reuse-window
                               display-buffer-same-window)))))
         (guix-result-message entries ',entry-type
                              search-type search-vals))

       (defun ,show-fun (entries)
         ,(concat "Display " entry-type-str " ENTRIES in the current " buf-str ".")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (,mode)
           (,(intern (concat "guix-" buf-type-str "-insert-entries"))
            entries ',entry-type)
           (goto-char (point-min))))

       (defun ,get-show-fun (search-type &rest search-vals)
         ,(concat "Search for " entry-str " and show results in the " buf-str ".\n"
                  "See `guix-get-entries' for the meaning of SEARCH-TYPE and\n"
                  "SEARCH-VALS.")
         (let ((entries (guix-get-entries ',entry-type search-type
                                          search-vals (,params-fun))))
           (,set-fun entries search-type search-vals))))))

(put 'guix-define-buffer-type 'lisp-indent-function 'defun)


;;; Messages

(defvar guix-messages
  '((package
     (id
      (0 "Packages not found.")
      (1 "")
      (many "%d packages." count))
     (name
      (0 "The package '%s' not found." val)
      (1 "A single package with name '%s'." val)
      (many "%d packages with '%s' name." count val))
     (regexp
      (0 "No packages matching '%s'." val)
      (1 "A single package matching '%s'." val)
      (many "%d packages matching '%s'." count val))
     (all-available
      (0 "No packages are available for some reason.")
      (1 "A single available package (that's strange).")
      (many "%d available packages." count))
     (newest-available
      (0 "No packages are available for some reason.")
      (1 "A single newest available package (that's strange).")
      (many "%d newest available packages." count))
     (installed
      (0 "No installed packages.")
      (1 "A single installed package.")
      (many "%d installed packages." count))
     (obsolete
      (0 "No obsolete packages.")
      (1 "A single obsolete package.")
      (many "%d obsolete packages." count))
     (generation
      (0 "No packages installed in generation %d." val)
      (1 "A single package installed in generation %d." val)
      (many "%d packages installed in generation %d." count val)))
    (generation
     (id
      (0 "Generations not found.")
      (1 "")
      (many "%d generations." count))
     (last
      (0 "No available generations.")
      (1 "The last generation.")
      (many "%d last generations." count))
     (all
      (0 "No available generations.")
      (1 "A single available generation.")
      (many "%d available generations." count)))))

(defun guix-result-message (entries entry-type search-type search-vals)
  "Display an appropriate message after displaying ENTRIES."
  (let* ((val (car search-vals))
         (count (length entries))
         (count-key (if (> count 1) 'many count))
         (msg-spec (guix-get-key-val guix-messages
                                     entry-type search-type count-key))
         (format (car msg-spec))
         (args (cdr msg-spec)))
    (mapc (lambda (subst)
            (setq args (cl-substitute (car subst) (cdr subst) args)))
          (list (cons count 'count)
                (cons val 'val)))
    (apply #'message format args)))


;;; Getting info about packages and generations

(defvar guix-search-entries-config
  '((package
     (id               . package-entries-by-ids)
     (name             . package-entries-by-spec)
     (regexp           . package-entries-by-regexp)
     (all-available    . all-available-package-entries)
     (newest-available . newest-available-package-entries)
     (installed        . installed-package-entries)
     (obsolete         . obsolete-package-entries)
     (generation       . generation-package-entries))
    (generation
     (id               . generation-entries-by-ids)
     (last             . last-generation-entries)
     (all              . all-generation-entries)))
  "Available methods for getting information.
Each element of the list has a form:

  (ENTRY-TYPE . ((SEARCH-TYPE . FUN) ...))

ENTRY-TYPE is a type of the entries to search.
SEARCH-TYPE is a search type for defining FUN.
FUN is a name of guile function used for searching.")

(defun guix-get-entries (entry-type search-type search-vals &optional params)
  "Search for entries of ENTRY-TYPE.

ENTRY-TYPE and SEARCH-TYPE define a search function from
`guix-search-entries-config' which is called with SEARCH-VALS as
arguments.

PARAMS is a list of parameters for receiving.  They are appended
to SEARCH-VALS.  If nil, get information with all available
parameters.

Returning value is a list of the form of `guix-entries'."
  (let ((fun (guix-get-key-val guix-search-entries-config
                               entry-type search-type)))
    (or fun (error "Wrong entry type '%S' or search type '%S'"
                   entry-type search-type))
    (guix-eval-read (apply #'guix-make-guile-expression
                           fun (append search-vals params)))))

(provide 'guix-base)

;;; guix-base.el ends here
