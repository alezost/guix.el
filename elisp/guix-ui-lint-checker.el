;;; guix-ui-lint-checker.el --- Interface for displaying package lint checkers  -*- lexical-binding: t -*-

;; Copyright © 2019 Alex Kost <alezost@gmail.com>

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

;; This file provides interface to display lint checkers of Guix
;; packages.

;;; Code:

(require 'bui)
(require 'guix nil t)
(require 'guix-repl)
(require 'guix-repl)
(require 'guix-guile)
(require 'guix-utils)
(require 'guix-read)
(require 'guix-package)

(guix-define-groups lint-checker)

(bui-define-entry-type guix-lint-checker
  :message-function 'guix-lint-checker-message)

(defun guix-lint-checker-get-entries (search-type &rest args)
  "Receive 'lint-checker' entries.
SEARCH-TYPE may be one of the following symbols: `all', `local',
`id', `name'."
  (guix-eval-read
   (apply #'guix-make-guile-expression
          'lint-checker-sexps search-type args)))

(defun guix-lint-checker-get-display (search-type &rest args)
  "Search for lint checkers and show results."
  (apply #'bui-list-get-display-entries
         'guix-lint-checker search-type args))

(defun guix-lint-checker-message (entries search-type &rest args)
  "Display a message after showing lint-checker ENTRIES."
  (when (null entries)
    (cond
     ((memq search-type '(all local))
      (message "Oops, lint checkers are not found for some reason."))
     ((memq search-type '(id name))
      (message "Lint checker '%s' not found." (car args))))))


;;; Lint-Checker 'list'

(bui-define-interface guix-lint-checker list
  :mode-name "Lint-Checker-List"
  :buffer-name "*Guix Lint Checkers*"
  :get-entries-function 'guix-lint-checker-get-entries
  :describe-function 'guix-lint-checker-list-describe
  :format '((name nil 30 t)
            (type nil 10 t)
            (description nil 50 t))
  :hint 'guix-lint-checker-list-hint)

(let ((map guix-lint-checker-list-mode-map))
  (define-key map (kbd "i") nil)
  (define-key map (kbd "RET") nil)
  (define-key map (kbd "L") 'guix-lint-checker-list-lint))

(defvar guix-lint-checker-list-default-hint
  '(("\\[guix-lint-checker-list-lint]") " lint packages;\n"))

(defun guix-lint-checker-list-hint ()
  (bui-format-hints
   guix-lint-checker-list-default-hint
   (bui-list-hint)
   bui-common-hint))

(defun guix-lint-checker-marked-or-current ()
  "Return names (strings) of the marked lint checkers."
  (mapcar #'symbol-name (bui-list-marked-or-current)))

(defun guix-lint-checker-list-lint (packages)
  "Lint PACKAGES with the marked lint checkers.
If there are no marked checkers, use checker on the current line.
Interactively, prompt for PACKAGES."
  (interactive
   (list (guix-read-package-names "Lint package,s: ")))
  (guix-package-lint packages
                     (guix-lint-checker-marked-or-current)))


;;; Interactive commands

(defvar guix-lint-checker-types
  '(all local network)
  "List of types used by `guix-lint-checkers'.")

(defun guix-lint-checker-read-type ()
  "Read lint checker type from minibuffer."
  (let ((type (guix-completing-read
               "Lint checker type: "
               (mapcar #'symbol-name
                       guix-lint-checker-types))))
    (and type (intern type))))

;;;###autoload
(defun guix-lint-checkers (&optional type)
  "Display lint checkers of the Guix packages.
TYPE should be one of the following symbols: `all', `local', `network'.
Interactively, with prefix argument, prompt for TYPE."
  (interactive
   (list (and current-prefix-arg
              (guix-lint-checker-read-type))))
  (guix-lint-checker-get-display (or type 'all)))

(provide 'guix-ui-lint-checker)

;;; guix-ui-lint-checker.el ends here
