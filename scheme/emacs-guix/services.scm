;;; services.scm --- Guix services

;; Copyright © 2017–2018 Alex Kost <alezost@gmail.com>

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
  #:use-module (guix i18n)
  #:use-module (guix ui)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:autoload   (gnu system) (operating-system-services)
  #:autoload   (guix scripts system) (read-operating-system)
  #:export (service-sexps))

(define service-id object-address)

(define service-type-name*
  (compose symbol->string service-type-name))

(define (service-type* service)
  "Return service-type of SERVICE.
SERVICE can be either a service object, or a service type itself."
  (if (service? service)
      (service-kind service)
      service))

;; XXX The same as `package-field-string' in (guix ui) module.
(define (texi-field->string object field-accessor)
  (and=> (field-accessor object)
         (compose texi->plain-text P_)))

(define-values (service-by-id
                register-service)
  (let ((table (delay (fold-service-types
                       (lambda (service table)
                         (vhash-consq (service-id service)
                                      service table))
                       vlist-null))))
    (values
     (lambda (id)
       "Return service by ID (its 'object-address') or #f."
       (match (vhash-assq id (force table))
         ((_ . service) service)
         (_ #f)))
     (lambda (service)
       "Register SERVICE by its 'object-address', so that later
'service-by-id' can be used to access it."
       (let ((table* (force table)))
         (set! table
               (delay (vhash-consq (service-id service)
                                   service table*))))))))

(define %service-param-alist
  `((id         . ,service-id)
    (name       . ,(lambda (service)
                     (service-type-name* (service-type* service))))
    (description . ,(lambda (service)
                      (texi-field->string (service-type* service)
                                          service-type-description)))
    (location   . ,(lambda (service)
                     (location->string
                      (service-type-location (service-type* service)))))
    (extensions . ,(lambda (service)
                     (map (compose service-type-name*
                                   service-extension-target)
                          (service-type-extensions (service-type* service)))))
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
    ((all)
     (fold-service-types cons '()))
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
SEARCH-TYPE should be one of the following symbols: 'id', 'all',
'from-os-file'."
  (let ((services (find-services search-type search-values))
        (->sexp (object-transformer %service-param-alist params)))
    (to-emacs-side (map ->sexp services))))

;;; services.scm ends here
