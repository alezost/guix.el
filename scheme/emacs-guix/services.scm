;;; services.scm --- Guix services

;; Copyright © 2017 Alex Kost <alezost@gmail.com>

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

;; This module provides the code to get info about Guix services defined
;; by (gnu services) module.

;;; Code:

(define-module (emacs-guix services)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (emacs-guix utils)
  #:autoload   (gnu system) (operating-system-services)
  #:autoload   (guix scripts system) (read-operating-system)
  #:export (service-sexps))

(define service-id object-address)

(define-values (service-by-id
                register-service)
  (let ((table vlist-null))
    (values
     (lambda (id)
       "Return service by ID (its 'object-address') or #f."
       (match (vhash-assq id table)
         ((_ . service) service)
         (_ #f)))
     (lambda (service)
       "Register SERVICE by its 'object-address', so that later
'service-by-id' can be used to access it."
       (set! table
             (vhash-consq (object-address service)
                          service table))))))

(define %service-param-alist
  `((id         . ,service-id)
    (type-name  . ,(lambda (service)
                     (service-type-name (service-kind service))))
    ;; (extensions . ,(lambda (service)
    ;;                  (map (lambda (ext)
    ;;                         (service-type-name
    ;;                          (service-extension-target ext)))
    ;;                       (service-type-extensions (service-kind service)))))
    ;; (parameters . ,(cut service-parameters <>))
    ))

(define (services-from-system-config-file file)
  "Return a list of services from system configuration FILE."
  (operating-system-services (read-operating-system file)))

(define (find-services search-type search-values)
  "Find services matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id)
     (filter-map service-by-id (list-maybe search-values)))
    ((from-os-file)
     (match search-values
       ((file)
        (let ((services (services-from-system-config-file file)))
          (map register-service services)
          services))
       (_ '())))
    (else
     (error (format #f "Wrong search type '~a' for services"
                    search-type)))))

(define (service-sexps search-type search-values params)
  "Return information (sexps) about services.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id',
'from-os-file'."
  (let ((services (find-services search-type search-values))
        (->sexp (object-transformer %service-param-alist params)))
    (map ->sexp services)))

;;; services.scm ends here
