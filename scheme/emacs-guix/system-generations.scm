;;; system-generations.scm --- Generations of system profiles

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

;; This module provides the code to get info about system generations of
;; GuixSD profiles.

;;; Code:

(define-module (emacs-guix system-generations)
  #:use-module (ice-9 match)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix memoization)
  #:use-module (guix profiles)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix generations)
  #:use-module (emacs-guix utils)
  #:export (system-generation-sexps))

(define (device->sexp device)
  (match device
    ((? file-system-label? label)
     (string-append (file-system-label->string label) " (label)"))
    ((? uuid? uuid)
     (string-append (uuid->string uuid) " (UUID)"))
    ((string? device) device)
    (_ "Unknown device type")))

(define system-generation-boot-parameters
  (mlambda (profile generation)
    "Return boot parameters for PROFILE's system GENERATION."
    (let* ((gen-file   (generation-file-name profile generation))
           (param-file (string-append gen-file "/parameters")))
      (call-with-input-file param-file read-boot-parameters))))

(define (system-generation-param-alist profile)
  "Return an alist of system generation parameters and procedures for
PROFILE."
  (define (accessor proc)
    (lambda (generation)
      (proc (system-generation-boot-parameters profile generation))))

  (append
   (generation-param-alist profile)
   `((label             . ,(accessor boot-parameters-label))
     (root-device       . ,(accessor (compose device->sexp
                                              boot-parameters-root-device)))
     (store-device      . ,(accessor (compose device->sexp
                                              boot-parameters-store-device)))
     (store-mount-point . ,(accessor boot-parameters-store-mount-point))
     (bootloader        . ,(accessor boot-parameters-bootloader-name))
     (kernel            . ,(accessor boot-parameters-kernel))
     (kernel-arguments  . ,(accessor boot-parameters-kernel-arguments))
     (initrd            . ,(accessor boot-parameters-initrd)))))

(define (system-generation-sexps profile search-type search-values params)
  "Return information (sexps) about system generations.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'last', 'all',
'time'.

PARAMS is a list of parameters for receiving.  If it is an empty list,
get information with all available parameters, which are: 'id',
'number', 'prev-number', 'file-name', 'time', 'label', 'root-device',
'kernel'."
  (let ((generations (find-generations profile search-type search-values))
        (->sexp (object-transformer (system-generation-param-alist profile)
                                    params)))
    (to-emacs-side (map ->sexp generations))))

;;; system-generations.scm ends here
