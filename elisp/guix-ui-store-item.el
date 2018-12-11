;;; guix-ui-store-item.el --- Interface for displaying store items  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

;; This file provides an interface to display store items in 'list' and
;; 'info' buffers.

;;; Code:

(require 'cl-lib)
(require 'ffap)
(require 'dash)
(require 'bui)
(require 'guix-package)
(require 'guix-guile)
(require 'guix-repl)
(require 'guix-misc)
(require 'guix-utils)
(require 'guix-auto-mode)  ; for regexps


;;; Misc functionality (move to "guix-store.el"?)

(defun guix-store-file-name-read ()
  "Read from minibuffer a store file name."
  (let* ((file (ffap-file-at-point))
         (file (and file
                    (string-match-p guix-store-directory file)
                    file)))
    ;; Read a string (not a file name), since using completions for
    ;; "/gnu/store" would probably be too much.
    (read-string "File from store: " file)))

(defvar guix-store-file-name-regexp
  (rx-to-string
   `(and ,guix-store-directory "/"
         (regexp ,guix-hash-regexp) "-"
         (group (* any)))
   t)
  "Regexp matching a string with store file name.
The first parenthesized group is the name itself (placed right
after the hash part).")

(defun guix-store-file-name< (a b)
  "Return non-nil if store file name A is less than B.
This is similar to `string<', except the '/gnu/store/...-' parts
of the file names are ignored."
  (cl-flet ((name (str)
              (and (string-match guix-store-file-name-regexp str)
                   (match-string 1 str))))
    (string-lessp (name a) (name b))))


;;; Common for both interfaces

(guix-define-groups store-item)

(bui-define-entry-type guix-store-item
  :message-function 'guix-store-item-message
  :titles '((id . "File name")
            (time . "Registration time")
            (number-of-derivers . "Derivers")
            (number-of-references . "References")
            (number-of-referrers . "Referrers")
            (number-of-requisites . "Requisites")))

(defcustom guix-store-item-show-total-size t
  "If non-nil, show total size after displaying store items."
  :type 'boolean
  :group 'guix-store-item)

(defface guix-store-item-invalid
  '((t :inherit font-lock-warning-face))
  "Face used for store items that are not valid."
  :group 'guix-store-item-faces)

(defun guix-store-item-get-entries (search-type
                                    &optional search-values params)
  "Receive 'store-item' entries.
SEARCH-TYPE may be one of the following symbols: `id', `live',
`dead', `referrers', `references', `derivers', `requisites',
`failures'."
  (guix-eval-read
   (guix-make-guile-expression
    'store-item-sexps search-type search-values params)))

(defun guix-store-item-get-display (search-type &rest search-values)
  "Search for store items and show results."
  (apply #'bui-list-get-display-entries
         'guix-store-item search-type search-values))

(defun guix-store-item-message (entries search-type &rest search-values)
  "Display a message after showing store item ENTRIES."
  (let ((count (length entries))
        (val (car search-values)))
    (cl-flet ((msg (str &rest args)
                (if guix-store-item-show-total-size
                    (apply #'message
                           (concat str "\nTotal size: %s.")
                           (-snoc args
                                  (guix-file-size-string
                                   (guix-store-item-entries-size entries))))
                  (apply #'message str args ))))
      (cl-case search-type
        ((id path)
         (cl-case count
           (0 (message "No info on the store item(s) found."))
           (1 (msg "Store item '%s'." val))
           (t (msg "%d store items displayed." count))))
        (live (msg "%d live store items." count))
        (dead (msg "%d dead store items." count))
        (failures
         (cl-case count
           (0 (message "No failures found."))
           (1 (msg "A single failure found."))
           (t (msg "%d failures found." count))))
        (t
         (let ((type (symbol-name search-type))
               (paths (mapconcat #'identity search-values ", ")))
           (cl-case count
             (0 (message "No %s of '%s' found." type paths))
             (1 (msg "A single %s of '%s'."
                     ;; Remove the trailing "s" from the search type
                     ;; ("derivers" -> "deriver").
                     (substring type 0 (1- (length type)))
                     paths))
             (t (msg "%d %s of '%s'." count type paths)))))))))

(defun guix-store-item-delete (&rest file-names)
  "Delete FILE-NAMES from the store."
  (or file-names
      (error "Nothing to delete"))
  (when (or (not guix-operation-confirm)
            (y-or-n-p
             (let ((count (length file-names)))
               (if (> count 1)
                   (format "Try to delete these %d store items? " count)
                 (format "Try to delete store item '%s'? "
                         (car file-names))))))
    (guix-eval-in-repl
     (apply #'guix-make-guile-expression
            'guix-command "gc" "--delete" file-names)
     (current-buffer))))

(defun guix-store-item-entries-size (entries)
  "Return total size of store item ENTRIES."
  (--reduce-from (+ acc
                    (or (bui-entry-non-void-value it 'size)
                        0))
                 0 entries))


;;; Store item 'info'

(bui-define-interface guix-store-item info
  :mode-name "Store-Item-Info"
  :buffer-name "*Guix Store Item Info*"
  :get-entries-function 'guix-store-item-info-get-entries
  :format '((id nil (guix-info-insert-file-name))
            nil
            guix-store-item-info-insert-invalid
            (size format guix-store-item-info-insert-size)
            (time format (time))
            (number-of-derivers
             format guix-store-item-info-insert-number-of-derivers)
            (number-of-references
             format guix-store-item-info-insert-number-of-references)
            (number-of-referrers
             format guix-store-item-info-insert-number-of-referrers)
            (number-of-requisites
             format guix-store-item-info-insert-number-of-requisites)))

(defvar guix-store-item-info-required-params
  '(id)
  "List of the required 'store-item' parameters.
These parameters are received from the Scheme side
along with the displayed parameters.

Do not remove `id' from this info as it is required for
identifying an entry.")

(defun guix-store-item-info-get-entries (search-type &rest search-values)
  "Return 'store-item' entries for displaying them in 'info' buffer."
  (guix-store-item-get-entries
   search-type search-values
   (cl-union guix-store-item-info-required-params
             (bui-info-displayed-params 'guix-store-item))))

(defun guix-info-insert-file-name (file-name)
  "Insert store item FILE-NAME at point."
  (bui-insert-button file-name 'bui-file)
  (bui-insert-indent)
  (bui-insert-action-button
   "Delete"
   (lambda (btn)
     (guix-store-item-delete (button-get btn 'file-name)))
   (format "Delete '%s' from the store" file-name)
   'file-name file-name))

(defun guix-store-item-info-insert-size (size entry)
  "Insert SIZE of the store item ENTRY at point."
  (bui-insert-non-nil size
    (insert (guix-file-size-string size))
    (bui-insert-indent)
    (let ((file-name (bui-entry-id entry)))
      (bui-insert-action-button
       "Size"
       (lambda (btn)
         (guix-package-size (button-get btn 'file-name)
                            (guix-read-package-size-type)))
       (format "Show full size info on '%s'" file-name)
       'file-name file-name))))

(defun guix-info-insert-store-item (file-name)
  "Insert store FILE-NAME at point."
  (bui-insert-button file-name 'bui-file)
  (bui-insert-indent)
  (bui-insert-action-button
   "Store item"
   (lambda (btn)
     (guix-store-item (button-get btn 'file-name)))
   (format "Show more info on %s" file-name)
   'file-name file-name))

(defun guix-info-insert-store-items (file-names)
  "Insert store FILE-NAMES at point.
FILE-NAMES can be a list or a single string."
  (bui-insert-non-nil file-names
    (dolist (file-name (guix-list-maybe file-names))
      (bui-newline)
      (bui-insert-indent)
      (guix-info-insert-store-item file-name))))

(defun guix-store-item-info-insert-invalid (entry)
  "Insert a text if the store item ENTRY is not valid."
  (when (bui-entry-non-void-value entry 'invalid)
    (if (not (file-exists-p (bui-entry-id entry)))
        (insert "This file does not exist.\n\n")
      (insert "Guix daemon says this path is ")
      (bui-format-insert "not valid" 'guix-store-item-invalid)
      (insert ".\nApparently, you may remove it from the store.\n\n"))))

(defun guix-store-item-info-insert-type-button (type entry)
  "Insert button to display TYPE of store item ENTRY at point.
TYPE should be one of the following symbols: `derivers',
`references', `referrers', `requisites'."
  (let ((file-name (bui-entry-id entry))
        (type-str (symbol-name type)))
    (bui-insert-action-button
     "Show"
     (lambda (btn)
       (guix-store-item-get-display (button-get btn 'search-type)
                                    (button-get btn 'file-name)))
     (format "Show %s of '%s'" type-str file-name)
     'search-type type
     'file-name file-name)))

(defmacro guix-store-item-info-define-insert-number (type)
  "Define a function to insert number of TYPE.
See `guix-store-item-info-insert-type-button' for the meaning of TYPE."
  (let* ((type-str (symbol-name type))
         (name (intern (concat "guix-store-item-info-insert-number-of-"
                               type-str)))
         (desc (concat "Insert NUMBER of " type-str
                       " of store item ENTRY at point.")))
    `(defun ,name (number entry)
       ,desc
       (bui-insert-non-nil number
         (bui-format-insert number)
         (unless (= 0 number)
           (bui-insert-indent)
           (guix-store-item-info-insert-type-button ',type entry))))))

(guix-store-item-info-define-insert-number derivers)
(guix-store-item-info-define-insert-number references)
(guix-store-item-info-define-insert-number referrers)
(guix-store-item-info-define-insert-number requisites)


;;; Store item 'list'

(bui-define-interface guix-store-item list
  :mode-name "Store-Item-List"
  :buffer-name "*Guix Store Items*"
  :get-entries-function 'guix-store-item-list-get-entries
  :describe-function 'guix-store-item-list-describe
  :format '((id guix-store-item-list-get-name 65
                guix-store-item-list-sort-file-names-0)
            (size nil 20 bui-list-sort-numerically-1 :right-align t))
  :hint 'guix-store-item-list-hint
  :sort-key '(size . t)
  :marks '((delete . ?D)))

(defvar guix-store-item-list-required-params
  '(id)
  "List of the required 'store-item' parameters.
These parameters are received from the Scheme side
along with the displayed parameters.

Do not remove `id' from this list as it is required for
identifying an entry.")

(let ((map guix-store-item-list-mode-map))
  (define-key map (kbd "e") 'guix-store-item-list-edit)
  (define-key map (kbd "d") 'guix-store-item-list-mark-delete)
  (define-key map (kbd "f") 'guix-store-item-list-referrers)
  (define-key map (kbd "F") 'guix-store-item-list-references)
  (define-key map (kbd "D") 'guix-store-item-list-derivers)
  (define-key map (kbd "R") 'guix-store-item-list-requisites)
  (define-key map (kbd "z") 'guix-store-item-list-size)
  (define-key map (kbd "x") 'guix-store-item-list-execute))

(defvar guix-store-item-list-default-hint
  '(("\\[guix-store-item-list-edit]") " go to the current store item;\n"
    ("\\[guix-store-item-list-derivers]") " show derivers; "
    ("\\[guix-store-item-list-requisites]") " show requisites;\n"
    ("\\[guix-store-item-list-referrers]") " show referrers; "
    ("\\[guix-store-item-list-references]") " show references;\n"
    ("\\[guix-store-item-list-mark-delete]") " mark for deletion; "
    ("\\[guix-store-item-list-execute]") " execute operation (deletions);\n"
    ("\\[guix-store-item-list-size]") " show size of the marked items;\n"))

(defun guix-store-item-list-hint ()
  (bui-format-hints
   guix-store-item-list-default-hint
   (bui-default-hint)))

(defun guix-store-item-list-get-entries (search-type &rest search-values)
  "Return 'store-item' entries for displaying them in 'list' buffer."
  (guix-store-item-get-entries
   search-type search-values
   (cl-union guix-store-item-list-required-params
             (bui-list-displayed-params 'guix-store-item))))

(defun guix-store-item-list-get-name (name entry)
  "Return NAME of the store item ENTRY.
Colorize it with an appropriate face if needed."
  (bui-get-string
   name
   (and (bui-entry-non-void-value entry 'invalid)
        'guix-store-item-invalid)))

(defun guix-store-item-list-sort-file-names-0 (a b)
  "Compare column 0 of tabulated entries A and B numerically.
This function is used for sort predicates for `tabulated-list-format'.
Return non-nil, if B is bigger than A."
  (cl-flet ((name (entry) (aref (cadr entry) 0)))
    (guix-store-file-name< (name a) (name b))))

(defun guix-store-item-list-describe (&rest ids)
  "Describe store-items with IDS (list of identifiers)."
  (bui-get-display-entries 'guix-store-item 'info (cons 'id ids)))

(defun guix-store-item-list-edit ()
  "Go to the current store item."
  (interactive)
  (guix-find-file (bui-list-current-id)))

(defun guix-store-item-list-mark-delete (&optional arg)
  "Mark the current store-item for deletion and move to the next line.
With ARG, mark all store-items for deletion."
  (interactive "P")
  (if arg
      (bui-list-mark-all 'delete)
    (bui-list--mark 'delete t)))

(defun guix-store-item-list-execute ()
  "Delete store items marked with '\\[guix-store-item-list-mark-delete]'."
  (interactive)
  (let ((marked (bui-list-get-marked-id-list 'delete)))
    (or marked
        (user-error "No store items marked for deletion"))
    (apply #'guix-store-item-delete marked)))

(defun guix-store-item-list-size ()
  "Show size of the marked (or current) store items.
Store items can be marked with any mark."
  (interactive)
  (let* ((marked (bui-list-marked-or-current))
         (count (length marked))
         (msg (if (= 1 count)
                  (format "Size of '%s': %%s." (car marked))
                (format "Size of %d marked items: %%s." count)))
         (size (guix-file-size-string
                (guix-store-item-entries-size
                 (bui-entries-by-ids (bui-current-entries) marked)))))
    (message msg size)))

(defmacro guix-store-item-list-define-show-items (type)
  "Define a function to show items by TYPE.
See `guix-store-item-list-insert-type-button' for the meaning of TYPE."
  (let* ((type-str (symbol-name type))
         (name (intern (concat "guix-store-item-list-" type-str)))
         (desc (concat "Display " type-str
                       " of the marked (or current) store items.")))
    `(defun ,name ()
       ,desc
       (interactive)
       (apply #'guix-store-item-get-display ',type
              (bui-list-marked-or-current 'general)))))

(guix-store-item-list-define-show-items derivers)
(guix-store-item-list-define-show-items references)
(guix-store-item-list-define-show-items referrers)
(guix-store-item-list-define-show-items requisites)


;;; Interactive commands

;;;###autoload
(defun guix-store-item (&rest file-names)
  "Display store items with FILE-NAMES.
Interactively, prompt for a single file name."
  (interactive (list (guix-store-file-name-read)))
  (apply #'guix-assert-files-exist file-names)
  (apply #'guix-store-item-get-display 'id file-names))

;;;###autoload
(defun guix-store-item-referrers (&rest file-names)
  "Display referrers of the FILE-NAMES store item.
This is analogous to 'guix gc --referrers FILE-NAMES' shell
command.  See Info node `(guix) Invoking guix gc'."
  (interactive (list (guix-store-file-name-read)))
  (apply #'guix-assert-files-exist file-names)
  (apply #'guix-store-item-get-display 'referrers file-names))

;;;###autoload
(defun guix-store-item-references (&rest file-names)
  "Display references of the FILE-NAMES store item.
This is analogous to 'guix gc --references FILE-NAMES' shell
command.  See Info node `(guix) Invoking guix gc'."
  (interactive (list (guix-store-file-name-read)))
  (apply #'guix-assert-files-exist file-names)
  (apply #'guix-store-item-get-display 'references file-names))

;;;###autoload
(defun guix-store-item-requisites (&rest file-names)
  "Display requisites of the FILE-NAMES store item.
This is analogous to 'guix gc --requisites FILE-NAMES' shell
command.  See Info node `(guix) Invoking guix gc'."
  (interactive (list (guix-store-file-name-read)))
  (apply #'guix-assert-files-exist file-names)
  (apply #'guix-store-item-get-display 'requisites file-names))

;;;###autoload
(defun guix-store-item-derivers (&rest file-names)
  "Display derivers of the FILE-NAMES store item.
This is analogous to 'guix gc --derivers FILE-NAMES' shell
command.  See Info node `(guix) Invoking guix gc'."
  (interactive (list (guix-store-file-name-read)))
  (apply #'guix-assert-files-exist file-names)
  (apply #'guix-store-item-get-display 'derivers file-names))

;;;###autoload
(defun guix-store-failures ()
  "Display store items corresponding to cached build failures.
This is analogous to 'guix gc --list-failures' shell command.
See Info node `(guix) Invoking guix gc'."
  (interactive)
  (guix-store-item-get-display 'failures))

;;;###autoload
(defun guix-store-live-items ()
  "Display live store items.
This is analogous to 'guix gc --list-live' shell command.
See Info node `(guix) Invoking guix gc'."
  (interactive)
  (guix-store-item-get-display 'live))

;;;###autoload
(defun guix-store-dead-items ()
  "Display dead store items.
This is analogous to 'guix gc --list-dead' shell command.
See Info node `(guix) Invoking guix gc'."
  (interactive)
  (guix-store-item-get-display 'dead))

(provide 'guix-ui-store-item)

;;; guix-ui-store-item.el ends here
