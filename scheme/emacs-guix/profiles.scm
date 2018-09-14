;;; profiles.scm --- Code related to Guix profiles

;; Copyright © 2017–2018 Alex Kost <alezost@gmail.com>
;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

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

;; This module provides the code related to profiles and manifests.

;;; Code:

(define-module (emacs-guix profiles)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:export (manifest-entry->name+version+output
            manifest-entries-by-name
            manifest-entry-by-output
            fold-manifest-by-name
            manifest-entry-dependencies-file-names
            search-paths-specifications
            search-paths))


;;; Manifest entries

(define (manifest-entry->name+version+output entry)
  (values
   (manifest-entry-name    entry)
   (manifest-entry-version entry)
   (manifest-entry-output  entry)))

(define (manifest-entries->hash-table entries)
  "Return a hash table of name keys and lists of matching manifest ENTRIES."
  (let ((table (make-hash-table (length entries))))
    (for-each (lambda (entry)
                (let* ((key (manifest-entry-name entry))
                       (ref (hash-ref table key)))
                  (hash-set! table key
                             (if ref (cons entry ref) (list entry)))))
              entries)
    table))

(define (manifest=? m1 m2)
  (or (eq? m1 m2)
      (equal? m1 m2)))

(define manifest->hash-table
  (let ((current-manifest #f)
        (current-table #f))
    (lambda (manifest)
      "Return a hash table of name keys and matching MANIFEST entries."
      (unless (manifest=? manifest current-manifest)
        (set! current-manifest manifest)
        (set! current-table (manifest-entries->hash-table
                             (manifest-entries manifest))))
      current-table)))

(define* (manifest-entries-by-name manifest name #:optional version output)
  "Return a list of MANIFEST entries matching NAME, VERSION and OUTPUT."
  (let ((entries (or (hash-ref (manifest->hash-table manifest) name)
                     '())))
    (if (or version output)
        (filter (lambda (entry)
                  (and (or (not version)
                           (equal? version (manifest-entry-version entry)))
                       (or (not output)
                           (equal? output  (manifest-entry-output entry)))))
                entries)
        entries)))

(define (manifest-entry-by-output entries output)
  "Return a manifest entry from ENTRIES matching OUTPUT."
  (find (lambda (entry)
          (string= output (manifest-entry-output entry)))
        entries))

(define (fold-manifest-by-name manifest proc init)
  "Fold over MANIFEST entries.
Call (PROC NAME VERSION ENTRIES RESULT), using INIT as the initial value
of RESULT.  ENTRIES is a list of manifest entries with NAME/VERSION."
  (hash-fold (lambda (name entries res)
               (proc name (manifest-entry-version (car entries))
                     entries res))
             init
             (manifest->hash-table manifest)))

(define (manifest-entry-dependencies-file-names entry)
  "Return store file names of manifest ENTRY dependencies."
  (map (match-lambda
         ((? manifest-entry? entry)
          (manifest-entry-item entry))
         ;; Before manifest version 3
         ;; <http://git.savannah.gnu.org/cgit/guix.git/commit/?id=55b4715fd4c03e46501f123c5c9bc6072edf12a4>,
         ;; 'manifest-entry-dependencies' returned a list of file names.
         ((? string? file-name) file-name)
         (_ "unknown dependency format"))
       (manifest-entry-dependencies entry)))


;;; Search paths

(define search-path-environment-variables
  ;; It is not exported from (guix scripts package) module.
  (@@ (guix scripts package) search-path-environment-variables))

(define* (search-paths profiles #:key (type 'exact))
  "Return a list with 'search paths' environment variables for PROFILES."
  (let* ((manifests (map profile-manifest profiles))
         (entries   (append-map manifest-transitive-entries
                                manifests)))
    (search-path-environment-variables
     entries profiles (const #f) #:kind type)))

(define (search-paths-specifications profile)
  "Return a list with 'search paths' specifications for PROFILE.
Each specification is (VARIABLE SEPARATOR PATH) list."
  (let ((specs (profile-search-paths profile)))
    (map (match-lambda
           ((spec . path)
            (list (search-path-specification-variable spec)
                  (search-path-specification-separator spec)
                  path)))
         specs)))

;;; profiles.scm ends here
