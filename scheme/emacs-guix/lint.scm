;;; lint.scm --- Code related to linting Guix packages

;; Copyright © 2015, 2017, 2019–2020 Alex Kost <alezost@gmail.com>

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
  #:use-module (guix lint)
  #:use-module (guix scripts lint)
  #:use-module (guix ui)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:autoload   (emacs-guix packages) (package-specification
                                      package-by-id-or-name)
  #:export (lint-checker-names
            lint-checker-sexps
            lint-package
            lint-packages))

(define (lint-checker-names)
  "Return a list of names of available lint checkers."
  (map (lambda (checker)
         (symbol->string (lint-checker-name checker)))
       %all-checkers))

(define (checkers-by-names names)
  "Return lint checkers by their NAMES (strings)."
  (let ((names (map string->symbol names)))
    (filter (lambda (checker)
              (memq (lint-checker-name checker)
                    names))
            %all-checkers)))

(define* (lint-package id-or-name #:optional (checkers '()))
  "Lint package with ID-OR-NAME using CHECKERS.
CHECKERS should be a list of strings (checker names).  If the list is
empty, use all available checkers."
  (let ((package (package-by-id-or-name id-or-name)))
    (if package
        (begin
          (run-checkers package
                        (if (null? checkers)
                            %all-checkers
                            (checkers-by-names checkers)))
          (format #t "Package '~a' checked.~%"
                  (package-specification package)))
        (format (current-error-port)
                "Couldn't find '~A' package~%"
                id-or-name))))

(define* (lint-packages names #:optional (checkers '()))
  "Lint packages with NAMES using CHECKERS.
See `lint-package' for details."
  (for-each (lambda (name)
              (lint-package name checkers))
            names))

(define (lint-checker-type checker)
  "Return a type of the CHECKER."
  (if (memq checker %local-checkers)
      'local
      'network))

(define %lint-checker-param-alist
  `((id . ,lint-checker-name)
    (name . ,lint-checker-name)
    (type . ,lint-checker-type)
    (description . ,lint-checker-description)))

(define lint-checker->sexp
  (object-transformer %lint-checker-param-alist))

(define (find-lint-checkers search-type . search-values)
  "Return a list of lint checkers depending on SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id name)
     (let ((names search-values))
       (checkers-by-names names)))
    ((local)
     %local-checkers)
    ((all)
     %all-checkers)))

(define (lint-checker-sexps search-type . search-values)
  (to-emacs-side
   (map lint-checker->sexp
        (apply find-lint-checkers search-type search-values))))

;;; lint.scm ends here
