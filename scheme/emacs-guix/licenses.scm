;;; licenses.scm --- Guix licenses

;; Copyright © 2016–2018 Alex Kost <alezost@gmail.com>

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

;; This module provides the code to get available licenses, return
;; license by name, etc.

;;; Code:

(define-module (emacs-guix licenses)
  #:use-module (srfi srfi-1)
  #:use-module (guix memoization)
  #:use-module (guix licenses)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:export (licenses
            license-names
            lookup-license
            lookup-license-uri
            license-sexps))

(define licenses
  (let ((ls (delay
              (filter license?
                      (module-map (lambda (_ var)
                                    (variable-ref var))
                                  (resolve-interface '(guix licenses)))))))
    (lambda ()
      "Return a list of available licenses."
      (force ls))))

(define (license-names)
  "Return a list of names of available licenses."
  (map license-name (licenses)))

(define lookup-license
  (mlambda (name)
    "Return a license by its name."
    (find (lambda (l)
            (string=? name (license-name l)))
          (licenses))))

(define (lookup-license-uri name)
  "Return a license URI by its name."
  (and=> (lookup-license name)
         license-uri))

(define %license-param-alist
  `((id      . ,license-name)
    (name    . ,license-name)
    (url     . ,license-uri)
    (comment . ,license-comment)))

(define license->sexp
  (object-transformer %license-param-alist))

(define (find-licenses search-type . search-values)
  "Return a list of licenses depending on SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id name)
     (let ((names search-values))
       (filter-map lookup-license names)))
    ((all)
     (licenses))))

(define (license-sexps search-type . search-values)
  (to-emacs-side
   (map license->sexp
        (apply find-licenses search-type search-values))))

;;; licenses.scm ends here
