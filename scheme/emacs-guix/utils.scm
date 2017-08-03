;;; utils.scm --- General utilities

;; Copyright © 2014–2017 Alex Kost <alezost@gmail.com>

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

(define-module (emacs-guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (first-or-false
            list-maybe
            search-load-path
            object-transformer))

(define-syntax-rule (first-or-false lst)
  (and (not (null? lst))
       (first lst)))

(define (list-maybe obj)
  (if (list? obj) obj (list obj)))

(define* (object-transformer param-alist #:optional (params '()))
  "Return procedure transforming objects into alist of parameter/value pairs.

PARAM-ALIST is alist of available parameters (symbols) and procedures
returning values of these parameters.  Each procedure is applied to
objects.

PARAMS is list of parameters from PARAM-ALIST that should be returned by
a resulting procedure.  If PARAMS is not specified or is an empty list,
use all available parameters.

Example:

  (let* ((alist `((plus1 . ,1+) (minus1 . ,1-) (mul2 . ,(cut * 2 <>))))
         (number->alist (object-transformer alist '(plus1 mul2))))
    (number->alist 8))
  =>
  ((plus1 . 9) (mul2 . 16))
"
  (let* ((use-all-params (null? params))
         (alist (filter-map (match-lambda
                             ((param . proc)
                              (and (or use-all-params
                                       (memq param params))
                                   (cons param proc)))
                             (_ #f))
                            param-alist)))
    (lambda objects
      (map (match-lambda
            ((param . proc)
             (cons param (apply proc objects))))
           alist))))

(define (search-load-path file-name)
  "Call (search-path %load-path FILE-NAME)."
  (search-path %load-path file-name))

;;; utils.scm ends here
