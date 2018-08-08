;;; actions.scm --- Various store actions on packages and generations

;; Copyright © 2014-2016 Alex Kost <alezost@gmail.com>

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

;; This module provides the code that interacts with guix-daemon to
;; build packages, delete generations, modify profiles and do other
;; store related actions.

;;; Code:

(define-module (emacs-guix actions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (guix grafts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:autoload   (guix scripts) (build-package)
  #:autoload   (guix scripts package) (build-and-use-profile
                                       delete-generations)
  #:use-module (emacs-guix utils)
  #:use-module (emacs-guix packages)
  #:export (process-package-actions
            build-package*
            delete-generations*
            package-store-path
            package-source-file-name
            package-source-build-derivation
            package-build-log-file))

;; See the commentary in (emacs-guix) module.
(current-build-output-port (current-error-port))

(define* (package->manifest-entry* package #:optional output)
  (and package
       (package->manifest-entry package output)))

(define* (make-install-manifest-entries id #:optional output)
  (package->manifest-entry* (package-by-id id) output))

(define* (make-upgrade-manifest-entries id #:optional output)
  (package->manifest-entry* (newest-package-by-id id) output))

(define* (make-manifest-pattern id #:optional output)
  "Make manifest pattern from a package ID and OUTPUT."
  (let-values (((name version)
                (id->name+version id)))
    (and name version
         (manifest-pattern
          (name name)
          (version version)
          (output output)))))

(define (convert-action-pattern pattern proc)
  "Convert action PATTERN into a list of objects returned by PROC.
PROC is called: (PROC ID) or (PROC ID OUTPUT)."
  (match pattern
    ((id . outputs)
     (if (null? outputs)
         (let ((obj (proc id)))
           (if obj (list obj) '()))
         (filter-map (cut proc id <>)
                     outputs)))
    (_ '())))

(define (convert-action-patterns patterns proc)
  (append-map (cut convert-action-pattern <> proc)
              patterns))

(define* (process-package-actions
          profile #:key (install '()) (upgrade '()) (remove '())
          (use-substitutes? #t) dry-run?)
  "Perform package actions.

INSTALL, UPGRADE, REMOVE are lists of 'package action patterns'.
Each pattern should have the following form:

  (ID . OUTPUTS)

ID is an object address or a full-name of a package.
OUTPUTS is a list of package outputs (may be an empty list)."
  (format #t "The process begins ...~%")
  (let* ((install (append
                   (convert-action-patterns
                    install make-install-manifest-entries)
                   (convert-action-patterns
                    upgrade make-upgrade-manifest-entries)))
         (remove (convert-action-patterns remove make-manifest-pattern))
         (transaction (manifest-transaction (install install)
                                            (remove remove)))
         (manifest (profile-manifest profile))
         (new-manifest (manifest-perform-transaction
                        manifest transaction)))
    (unless (and (null? install) (null? remove))
      (parameterize ((%graft? (not dry-run?)))
        (with-store store
          (set-build-options store
                             #:print-build-trace #f
                             #:use-substitutes? use-substitutes?)
          (show-manifest-transaction store manifest transaction
                                     #:dry-run? dry-run?)
          (build-and-use-profile store profile new-manifest
                                 #:use-substitutes? use-substitutes?
                                 #:dry-run? dry-run?))))))

(define (build-package* package . build-options)
  "Build PACKAGE using BUILD-OPTIONS acceptable by 'set-build-options'.
Show what and how will/would be built."
  (with-store store
    (run-with-store store
      (apply build-package package build-options))))

(define (delete-generations* profile generations)
  "Delete GENERATIONS from PROFILE.
GENERATIONS is a list of generation numbers."
  (with-store store
    (delete-generations store profile generations)))

(define (package-store-path package-id)
  "Return a list of store directories of outputs of package PACKAGE-ID."
  (match (package-by-id package-id)
    (#f '())
    (package
      (with-store store
        (map (match-lambda
               ((_ . drv)
                (derivation-output-path drv)))
             (derivation-outputs (package-derivation store package)))))))

(define (package-source-derivation->store-file-name derivation)
  "Return a store file name of the package source DERIVATION."
  (match (derivation-outputs derivation)
    ;; Source derivation is always (("out" . derivation)).
    (((_ . output-drv))
     (derivation-output-path output-drv))
    (_ #f)))

(define (package-source-file-name package-id)
  "Return a store file name to a source of a package PACKAGE-ID."
  (and-let* ((package (package-by-id package-id))
             (source  (package-source package)))
    (with-store store
      (package-source-derivation->store-file-name
       (package-source-derivation store source)))))

(define* (package-source-build-derivation package-id #:key dry-run?
                                          (use-substitutes? #t))
  "Build source derivation of a package PACKAGE-ID."
  (and-let* ((package (package-by-id package-id))
             (source  (package-source package)))
    (with-store store
      (let* ((derivation  (package-source-derivation store source))
             (derivations (list derivation)))
        (set-build-options store
                           #:print-build-trace #f
                           #:use-substitutes? use-substitutes?)
        (show-what-to-build store derivations
                            #:use-substitutes? use-substitutes?
                            #:dry-run? dry-run?)
        (unless dry-run?
          (build-derivations store derivations))
        (format #t "The source store file name: ~a~%"
                (package-source-derivation->store-file-name
                 derivation))))))

(define (package-build-log-file package-id)
  "Return the build log file of a package PACKAGE-ID.
Return #f if the build log is not found."
  (and-let* ((package (package-by-id package-id)))
    (with-store store
      (let* ((derivation (package-derivation store package))
             (file       (derivation-file-name derivation)))
        (or (log-file store file)
            ((@@ (guix scripts build) log-url) store file))))))

;;; actions.scm ends here
