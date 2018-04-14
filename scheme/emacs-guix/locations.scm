;;; locations.scm --- Locations of Guix package

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

;; This file provides code to get packages by their locations, etc.

;;; Code:

(define-module (emacs-guix locations)
  #:use-module (ice-9 vlist)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:autoload   (guix ui) (location->string)
  #:use-module (guix utils)
  #:autoload   (emacs-guix packages) (package-by-id-or-name)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:export (packages-by-location-file
            package-location-string
            package-location-files
            package-location-sexps))

(define (package-location-string id-or-name)
  "Return a location string of a package with ID-OR-NAME."
  (and=> (package-by-id-or-name id-or-name)
         (compose location->string package-location)))

(define-values (packages-by-location-file
                package-location-files)
  (let* ((table (delay (fold-packages
                        (lambda (package table)
                          (let ((file (location-file
                                       (package-location package))))
                            (vhash-cons file package table)))
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
       "Return a list of packages defined in location FILE."
       (vhash-fold* cons '() file (force table)))
     (lambda ()
       "Return a list of file names of all package locations."
       (force files)))))

(define %package-location-param-alist
  `((id       . ,identity)
    (location . ,identity)
    (number-of-packages . ,(lambda (location)
                             (length (packages-by-location-file location))))))

(define package-location->sexp
  (object-transformer %package-location-param-alist))

(define (package-location-sexps)
  (to-emacs-side
   (map package-location->sexp (package-location-files))))

;;; locations.scm ends here
