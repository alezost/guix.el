;;; system.scm --- Guix operating-system

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

;; This module provides the code to get info about 'operating-system'
;; declarations.

;;; Code:

(define-module (emacs-guix system)
  #:use-module (gnu system)
  #:use-module (gnu bootloader)
  #:use-module (guix utils)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix utils)
  #:autoload   (emacs-guix packages) (package-specification
                                      register-package)
  #:autoload   (guix scripts system) (read-operating-system)
  #:export (system-sexps))

(define (system-kernel-sexp os)
  "Return sexp for the kernel package of OS."
  (let ((kernel (operating-system-kernel os)))
    (register-package kernel)
    (list (object-address kernel)
          (package-specification kernel))))

(define (system-bootloader-sexp os)
  "Return sexp for the bootloader of OS."
  (let ((pkg (bootloader-package (bootloader-configuration-bootloader
                                  (operating-system-bootloader os)))))
    (package-specification pkg)))

(define (system-firmware-sexp os)
  "Return sexp for the firmware packages of OS."
  (map package-specification
       ;; 'operating-system-firmware' is not exported.
       ((@@ (gnu system) operating-system-firmware) os)))

(define %system-param-alist
  `((kernel             . ,system-kernel-sexp)
    (initrd-modules     . ,operating-system-initrd-modules)
    (bootloader         . ,system-bootloader-sexp)
    (firmware           . ,system-firmware-sexp)
    (number-of-packages . ,(compose length operating-system-packages))
    ;; XXX Why does 'compose' work for packages but not for services??
    ;; (number-of-services . ,(compose length operating-system-services))
    (number-of-services . ,(lambda (system)
                             (length (operating-system-services system))))))

(define (find-system search-type search-values)
  "Find system matching SEARCH-TYPE and SEARCH-VALUES."
  (case search-type
    ((from-file)
     (list (read-operating-system (car search-values))))
    ;; ((from-expression)
    ;;  (register/return-system (read-eval (car search-values))))
    (else
     (error (format #f "Wrong search type '~a' for system"
                    search-type)))))

(define (system-sexps search-type search-values params)
  "Return information (sexps) about system.
SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'from-file'."
  (let ((system (find-system search-type search-values))
        (->sexp (object-transformer %system-param-alist params)))
    (to-emacs-side (map ->sexp system))))

;;; system.scm ends here
