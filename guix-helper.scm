;;; guix-helper.scm --- Scheme code for guix.el package

;; Copyright © 2014 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;; Setting paths

(use-modules (srfi srfi-26))

(define guix-dir)

;; The code is taken from ‘guix’ executable script
(define (set-paths!)
  (define-syntax-rule (push! elt v) (set! v (cons elt v)))
  (let ((module-dir (%site-dir))
        (updates-dir (and=> (or (getenv "XDG_CONFIG_HOME")
                                (and=> (getenv "HOME")
                                       (cut string-append <> "/.config")))
                            (cut string-append <> "/guix/latest"))))
    (set! guix-dir (if (and updates-dir (file-exists? updates-dir))
                       updates-dir
                       module-dir))
    (push! guix-dir %load-path)
    (push! guix-dir %load-compiled-path)))

(set-paths!)


(use-modules
 (ice-9 vlist)
 (srfi srfi-1)
 (srfi srfi-11)
 (guix packages)
 (guix profiles)
 (guix ui)
 (guix licenses)
 (gnu packages))

(define current-manifest)

(define (set-current-manifest!)
  (set! current-manifest
        ;; Is this path always correct?
        (profile-manifest (string-append (getenv "HOME")
                                         "/.guix-profile"))))
(set-current-manifest!)

(define (funcall-or-map fun obj)
  (if (list? obj)
      (map fun obj)
      (fun obj)))

(define (get-info object fields field-alist)
  "Return info about the OBJECT.

FIELD-ALIST is alist of fields (symbols) and functions.  Each function
should accept OBJECT as a single argument.

FIELDS are fields from FIELD-ALIST.  If FIELDS are not specified, use
all available fields.

For each field from FIELDS a corresponding function is called on OBJECT.
Returning value is alist of FIELDS and the values of funcalls."
  (let ((use-all-fields (null? fields)))
    (fold (lambda (field-assoc res)
            (let ((field (car field-assoc))
                  (fun   (cdr field-assoc)))
              (if (or use-all-fields
                      (memq field fields))
                  (cons (cons field (fun object)) res)
                  res)))
          '()
          field-alist)))

(define package-installed-field-alist
  (list
   (cons 'output        manifest-entry-output)
   (cons 'path          manifest-entry-path)
   (cons 'dependencies  manifest-entry-dependencies)))

(define (package-installed-info package . fields)
  "Return additional info about the PACKAGE if it is installed.
FIELDS are field names from `package-installed-field-alist', if FIELDS
are not specified, use all fields.
Returning value is list of alists of names and values of package fields."
  (let* ((pattern (manifest-pattern (name (package-name package))
                                    (version (package-version package))
                                    (output #f)))
         (entries (manifest-matching-entries current-manifest
                                             (list pattern))))
    (map (lambda (entry)
           (get-info entry fields package-installed-field-alist))
         entries)))

(define (package-inputs-names inputs)
  "Return list of full names of the packages from INPUTS.
INPUTS can be either package inputs or native inputs."
  (fold (lambda (input res)
          (let ((pkg (cadr input)))
            (if (package? pkg)
                (cons (package-full-name pkg) res)
                res)))
        '()
        inputs))

(define (package-license-names package)
  "Return license name or list of license names of the PACKAGE."
  (funcall-or-map (lambda (l)
                    (and (license? l) (license-name l)))
                  (package-license package)))

(define package-field-alist
  (list
   (cons 'name          package-name)
   (cons 'version       package-version)
   (cons 'license       package-license-names)
   (cons 'synopsis      package-synopsis)
   (cons 'description   package-description)
   (cons 'home-url      package-home-page)
   (cons 'outputs       package-outputs)
   (cons 'inputs        (lambda (pkg) (package-inputs-names
                                  (package-inputs pkg))))
   (cons 'native-inputs (lambda (pkg) (package-inputs-names
                                  (package-native-inputs pkg))))
   (cons 'location      (lambda (pkg) (location->string
                                  (package-location pkg))))
   (cons 'installed     package-installed-info)))

(define (package-field-accessor field)
  "Return the procedure of a package FIELD from `package-field-alist'."
  (cdr (assq field package-field-alist)))

(define (package-field package field)
  "Return the value of a PACKAGE FIELD."
  ((package-field-accessor field) package))

(define (package-info package . fields)
  "Return info about the PACKAGE.
FIELDS are field names from `package-field-alist', if FIELDS are not
specified, use all fields.
Returning value is alist of names and values of package fields."
  (get-info package fields package-field-alist))

(define (find-packages-by-spec spec . fields)
  "Search for packages by name specification SPEC.
Return list of package info.
For the meaning of FIELDS, see `package-info'."
  (set-current-manifest!)
  (let-values (((name version out)
                (package-specification->name+version+output spec)))
    (map (lambda (pkg)
           (apply package-info pkg fields))
         (find-packages-by-name name version))))

(define (package-match? regexp package fields)
  "Return #t if REGEXP matches any PACKAGE field from FIELDS."
  (any (lambda (field)
         (let ((val (package-field package field)))
           (and (string? val) (regexp-exec regexp val))))
       fields))

(define (find-packages-by-regexp regexp match-fields . ret-fields)
  "Search for packages by REGEXP string.
MATCH-FIELDS is a list of fields that REGEXP can match.
For the meaning of RET-FIELDS, see `package-info'.
Return list of package info."
  (set-current-manifest!)
  (let ((re (make-regexp regexp regexp/icase)))
    (fold-packages (lambda (pkg res)
                     (if (package-match? re pkg match-fields)
                         (cons (apply package-info pkg ret-fields) res)
                         res))
                   '())))

(define (newest-available-packages . fields)
  "Return list of package info for the newest available packages.
For the meaning of FIELDS, see `package-info'."
  (set-current-manifest!)
  (vhash-fold (lambda (key val res)
                (cons (apply package-info (cadr val) fields) res))
              '()
              (find-newest-available-packages)))

(define (all-available-packages . fields)
  "Return list of package info for all available packages.
For the meaning of FIELDS, see `package-info'."
  (set-current-manifest!)
  (fold-packages (lambda (pkg res)
                   (cons (apply package-info pkg fields) res))
                 '()))

