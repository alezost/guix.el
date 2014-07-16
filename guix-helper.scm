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

(use-modules
 (ice-9 vlist)
 (srfi srfi-1)
 (srfi srfi-11)
 (gnu packages)
 (guix packages)
 (guix ui)
 (guix licenses))

(define (funcall-or-map fun obj)
  (if (list? obj)
      (map fun obj)
      (fun obj)))

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
                                  (package-location pkg))))))

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
  (let ((use-all-fields (null? fields)))
    (let loop ((field-alist package-field-alist)
               (res '()))
      (if (null? field-alist)
          res
          (let* ((field-elem (car field-alist))
                 (field (car field-elem))
                 (fun   (cdr field-elem)))
            (loop (cdr field-alist)
                  (if (or use-all-fields
                          (memq field fields))
                      (cons (cons field (fun package)) res)
                      res)))))))

(define (find-packages-by-spec spec . fields)
  "Search for packages by name specification SPEC.
Return list of package info.
For the meaning of FIELDS, see `package-info'."
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
  (let ((re (make-regexp regexp regexp/icase)))
    (fold-packages (lambda (pkg res)
                     (if (package-match? re pkg match-fields)
                         (cons (apply package-info pkg ret-fields) res)
                         res))
                   '())))

(define (newest-available-packages . fields)
  "Return list of package info for the newest available packages.
For the meaning of FIELDS, see `package-info'."
  (vhash-fold (lambda (key val res)
                (cons (apply package-info (cadr val) fields) res))
              '()
              (find-newest-available-packages)))

(define (all-available-packages . fields)
  "Return list of package info for all available packages.
For the meaning of FIELDS, see `package-info'."
  (fold-packages (lambda (pkg res)
                   (cons (apply package-info pkg fields) res))
                 '()))

