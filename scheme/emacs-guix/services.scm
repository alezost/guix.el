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
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix i18n)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:autoload   (gnu system) (operating-system-services)
  #:autoload   (guix scripts system) (read-operating-system)
  #:export (service-names
            service-names*
            service-sexps
            service-location-string
            service-location-files
            service-location-sexps))

(define service-id object-address)

(define service-type-name*
  (compose symbol->string service-type-name))

(define (service-type* service)
  "Return service-type of SERVICE.
SERVICE can be either a service object, or a service type itself."
  (if (service? service)
      (service-kind service)
      service))

(define (shepherd-services service-or-type)
  "Return the list of Shepherd services created by SERVICE-OR-TYPE."
  (let-values (((type service)
                (if (service? service-or-type)
                    (values (service-kind service-or-type)
                            service-or-type)
                    (values service-or-type
                            (guard (c ((service-error? c) #f))
                              (service service-or-type))))))
    (match service
      (#f '())
      ((? service? service)
       (let* ((extension (find (lambda (extension)
                                 (eq? (service-extension-target extension)
                                      shepherd-root-service-type))
                               (service-type-extensions type)))
              (compute   (and extension
                              (service-extension-compute extension))))
         (if compute
             (compute (service-value service))
             '()))))))

(define %shepherd-service-param-alist
  `((names         . ,shepherd-service-provision)
    (documentation . ,shepherd-service-documentation)
    (requirements  . ,shepherd-service-requirement)))

(define shepherd-service->sexp
  (object-transformer %shepherd-service-param-alist))

(define (shepherd-services->sexp services)
  (map shepherd-service->sexp services))

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

(define (register/return-services services)
  "Call `register-service' on SERVICES and return them."
  (map register-service services)
  services)

(define-values (service-names
                services-by-name)
  (let ((table (delay (fold-service-types
                       (lambda (type table)
                         (vhash-consq (service-type-name type)
                                      type table))
                       vlist-null))))
    (values
     (lambda ()
       "Return a list of names (symbols) of available service types."
       (vhash-fold (lambda (name _ result)
                     (cons name result))
                   '()
                   (force table)))
     (lambda (name)
       "Return services matching NAME."
       (vhash-foldq* cons '()
                     (string->symbol* name)
                     (force table))))))

(define (service-by-id-or-name id-or-name)
  "Return service object by ID-OR-NAME.
ID-OR-NAME may be either a service ID (object address) or its name."
  (or (service-by-id id-or-name)
      (and (string? id-or-name)
           (match (services-by-name id-or-name)
             (()              #f)
             ((service _ ...) service)))))

(define (services-by-regexp regexp match-params)
  "Return a list of services matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match."
  (define (service-match? service regexp)
    (any (lambda (param)
           (let ((val (service-param service param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (let ((re (make-regexp regexp regexp/icase)))
    (fold-service-types (lambda (type result)
                          (if (service-match? type re)
                              (cons type result)
                              result))
                        '())))

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
    (shepherd   . ,(compose shepherd-services->sexp
                            shepherd-services))))

(define (service-param service param)
  "Return a value of a SERVICE PARAM."
  (and=> (assq-ref %service-param-alist param)
         (cut <> service)))

(define (services-from-system-config-file file)
  "Return a list of services from system configuration FILE."
  (operating-system-services (read-operating-system file)))

(define (find-services search-type search-values)
  "Find services matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id)
     (filter-map service-by-id (list-maybe search-values)))
    ((name)
     (services-by-name (car search-values)))
    ((regexp)
     (match search-values
       ((regexp params)
        (services-by-regexp regexp params))
       (_ '())))
    ((location)
     (services-by-location-file (car search-values)))
    ((all)
     (fold-service-types cons '()))
    ((from-expression)
     (register/return-services (read-eval (car search-values))))
    ((from-os-file)
     (register/return-services
      (services-from-system-config-file (car search-values))))
    (else
     (error (format #f "Wrong search type '~a' for services"
                    search-type)))))

(define (service-sexps search-type search-values params)
  "Return information (sexps) about services.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'name', 'all',
'regexp', 'location', 'from-os-file', 'from-expression'."
  (let ((services (find-services search-type search-values))
        (->sexp (object-transformer %service-param-alist params)))
    (to-emacs-side (map ->sexp services))))

(define (service-names*)
  "Return to emacs side a list of names (strings) of available services."
  (to-emacs-side
   (map symbol->string (service-names))))


;;; Service locations

;; TODO The code below is almost the same as the "package locations"
;; code.  It should be generalized!

(define (service-location-string id-or-name)
  "Return location string of a service with ID-OR-NAME."
  (and=> (service-by-id-or-name id-or-name)
         (compose location->string service-type-location)))

(define-values (services-by-location-file
                service-location-files)
  (let* ((table (delay (fold-service-types
                        (lambda (type table)
                          (let ((file (location-file
                                       (service-type-location type))))
                            (vhash-cons file type table)))
                        vlist-null)))
         (files (delay (vhash-fold
                        (lambda (file _ result)
                          (if (member file result)
                              result
                              (cons file result)))
                        '()
                        (force table)))))
    (values
     (lambda (file)
       "Return a list of services defined in location FILE."
       (vhash-fold* cons '() file (force table)))
     (lambda ()
       "Return a list of file names of all service locations."
       (force files)))))

(define %service-location-param-alist
  `((id       . ,identity)
    (location . ,identity)
    (number-of-services . ,(lambda (location)
                             (length (services-by-location-file
                                      location))))))

(define service-location->sexp
  (object-transformer %service-location-param-alist))

(define (service-location-sexps)
  (to-emacs-side
   (map service-location->sexp (service-location-files))))

;;; services.scm ends here
