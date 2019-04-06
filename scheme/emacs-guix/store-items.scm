;;; store-items.scm --- Guix store items

;; Copyright © 2018–2019 Alex Kost <alezost@gmail.com>

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
  #:use-module (ice-9 match)
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
                     (requisites store (list item))))
    (number-of-derivers   . ,(compose length (cut valid-derivers store <>)))
    (number-of-references . ,(compose length (cut references store <>)))
    (number-of-referrers  . ,(compose length (cut referrers store <>)))
    (number-of-requisites . ,(lambda (item)
                               (length (requisites store (list item)))))))

(define %path-info-param-alist
  `((size       . ,path-info-nar-size)
    ;; XXX This is not the hash of a store file, so what is it?
    (hash       . ,(compose bytevector->nix-base32-string
                            path-info-hash))
    (deriver    . ,path-info-deriver)
    (time       . ,path-info-registration-time)))

(define (relatives relatives paths)
  "Return a list of RELATIVES of PATHS.
RELATIVES should be a procedure taking a path as argument."
  (match paths
    ((path)
     (relatives path))
    ((paths ...)
     (delete-duplicates (append-map relatives paths)))
    (_ '())))

(define (store-items store search-type search-values)
  "Find STORE items matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id path)
     search-values)
    ((referrers)
     (relatives (cut referrers store <>) search-values))
    ((references)
     (relatives (cut references store <>) search-values))
    ((derivers)
     (relatives (cut valid-derivers store <>) search-values))
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
  "Like 'query-path-info' but returns #f if there is store protocol error."
  (guard (c ((store-protocol-error? c)
             (format (current-warning-port)
                     "error in query-path-info: ~a~%" c)
             #f))
    (query-path-info server path)))

(define (store-item-sexps search-type search-values params)
  "Return information (sexps) about store items.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'live', 'dead',
'referrers', 'references', 'derivers', 'requisites', 'failures'."
  (define check-size
    (or (null? params)
        (memq 'size params)))

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
                    (let ((base-sexp `((id . ,item)
                                       (invalid . t))))
                      (if check-size
                          (cons (cons 'size (file-size item))
                                base-sexp)
                          base-sexp)))))
            (store-items store search-type search-values))))))

;;; store-items.scm ends here
