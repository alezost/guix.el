;;; store-items.scm --- Guix store items

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

;; This module provides the code to get info about Guix store items.

;;; Code:

(define-module (emacs-guix store-items)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix base32)
  #:use-module (guix store)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:export (store-item-sexps))

(define (store-item-param-alist store)
  `((id         . ,identity)
    (derivers   . ,(cut valid-derivers store <>))
    (references . ,(cut references store <>))
    (referrers  . ,(cut referrers store <>))
    (requisites . ,(lambda (item)
                     (requisites store (list item))))))

(define %path-info-param-alist
  `((size       . ,path-info-nar-size)
    ;; XXX This is not the hash of a store file, so what is it?
    (hash       . ,(compose bytevector->nix-base32-string
                            path-info-hash))
    (deriver    . ,path-info-deriver)
    (time       . ,path-info-registration-time)))

(define (store-items store search-type search-values)
  "Find STORE items matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id path)
     search-values)
    ((referrers)
     (referrers store (car search-values)))
    ((references)
     (references store (car search-values)))
    ((derivers)
     (valid-derivers store (car search-values)))
    ((requisites)
     (requisites store search-values))
    ((failures)
     (query-failed-paths store))
    ((live)
     (live-paths store))
    ((dead)
     (dead-paths store))
    (else
     (error (format #f "Wrong search type '~a' for store items"
                    search-type)))))

(define (query-path-info* server path)
  "Like 'query-path-info' but returns #f if there is nix protocol error."
  (guard (c ((nix-protocol-error? c)
             (format (current-warning-port)
                     "error in query-path-info: ~a~%" c)
             #f))
    (query-path-info server path)))

(define (store-item-sexps search-type search-values params)
  "Return information (sexps) about store items.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'live', 'dead',
'referrers', 'references', 'derivers', 'requisites', 'failures'."
  (to-emacs-side
   (with-store store
     (let ((item-alist (store-item-param-alist store)))
       (map (lambda (item)
              (let ((info (query-path-info* store item)))
                (if info
                    ;; sexp for valid item.
                    (append ((object-transformer item-alist params)
                             item)
                            ((object-transformer
                              %path-info-param-alist params)
                             info))
                    ;; sexp for invalid item.
                    `((id . ,item)
                      (invalid . t)))))
            (store-items store search-type search-values))))))

;;; store-items.scm ends here
