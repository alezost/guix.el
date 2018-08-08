;;; lint.scm --- Code related to linting Guix packages

;; Copyright © 2015, 2017 Alex Kost <alezost@gmail.com>

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

;;; Code:

(define-module (emacs-guix lint)
  #:use-module (guix scripts lint)
  #:use-module (guix ui)
  #:autoload   (emacs-guix packages) (package-by-id-or-name)
  #:export (lint-checker-names
            lint-package))

(define (lint-checker-names)
  "Return a list of names of available lint checkers."
  (map (lambda (checker)
         (symbol->string (lint-checker-name checker)))
       %checkers))

(define (checkers-by-names names)
  "Return lint checkers by their NAMES (strings)."
  (let ((names (map string->symbol names)))
    (filter (lambda (checker)
              (memq (lint-checker-name checker)
                    names))
            %checkers)))

(define* (lint-package id-or-name #:optional (checkers '()))
  "Lint package with ID-OR-NAME using CHECKERS.
CHECKERS should be a list of strings (checker names).  If the list is
empty, use all available checkers."
  (let ((package (package-by-id-or-name id-or-name)))
    (if package
        (begin
          (run-checkers package
                        (if (null? checkers)
                            %checkers
                            (checkers-by-names checkers)))
          (display "Package checking completed.")
          (newline))
        (format (current-error-port)
                "Couldn't find '~A' package~%"
                id-or-name))))

;;; lint.scm ends here
