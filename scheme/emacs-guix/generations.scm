;;; generations.scm --- Generations of profiles

;; Copyright © 2014–2016, 2018 Alex Kost <alezost@gmail.com>

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

;; This module provides the code to get info about generations of Guix
;; profiles.

;;; Code:

(define-module (emacs-guix generations)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (guix profiles)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:export (generation-param-alist
            matching-generations
            find-generations
            last-generations
            generation-sexps))

(define (generation-param-alist profile)
  "Return an alist of generation parameters and procedures for PROFILE."
  (let ((current (generation-number profile)))
    `((id          . ,identity)
      (number      . ,identity)
      (prev-number . ,(cut previous-generation-number profile <>))
      (current     . ,(cut = current <>))
      (file-name   . ,(cut generation-file-name profile <>))
      (time        . ,(lambda (gen)
                        (time-second (generation-time profile gen)))))))

(define (matching-generations profile predicate)
  "Return a list of PROFILE generations matching PREDICATE."
  (filter predicate (profile-generations profile)))

(define (last-generations profile number)
  "Return a list of last NUMBER generations.
If NUMBER is 0 or less, return all generations."
  (let ((generations (profile-generations profile))
        (number (if (<= number 0) +inf.0 number)))
    (if (> (length generations) number)
        (list-head  (reverse generations) number)
        generations)))

(define (find-generations profile search-type search-values)
  "Find PROFILE's generations matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((id)
     (matching-generations profile (cut memq <> search-values)))
    ((last)
     (last-generations profile (car search-values)))
    ((all)
     (last-generations profile +inf.0))
    ((time)
     (match search-values
       ((from to)
        (matching-generations
         profile
         (lambda (gen)
           (let ((time (time-second (generation-time profile gen))))
             (< from time to)))))
       (_ '())))
    (else
     (error (format #f "Wrong search type '~a' for generations"
                    search-type)))))

(define (generation-sexps profile search-type search-values params)
  "Return information (sexps) about generations.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'last', 'all',
'time'.

PARAMS is a list of parameters for receiving.  If it is an empty list,
get information with all available parameters, which are: 'id',
'number', 'prev-number', 'file-name', 'time'."
  (let ((generations (find-generations profile search-type search-values))
        (->sexp (object-transformer (generation-param-alist profile)
                                    params)))
    (to-emacs-side (map ->sexp generations))))

;;; generations.scm ends here
