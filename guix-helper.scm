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

;;; Commentary:

;; The following functions may be called by elisp part to get
;; information about the packages:
;;
;; - packages-info-by-keys
;; - packages-info-by-spec
;; - packages-info-by-regexp
;; - all-available-packages-info
;; - newest-available-packages-info
;; - installed-packages-info
;; - obsolete-packages-info
;;
;; Returning value of the above functions is a list of "packages info".
;; Each package info is alist of package paramers (such as ‘name’ or
;; ‘version’) and their values (see `package-param-alist' and
;; `package-info').
;;
;; Since name/version pair is not necessarily unique, we use
;; `object-address' to identify a package (for ‘id’ parameter), if
;; possible.  However for the obsolete packages (that can be found in
;; installed manifest but not in a package directory), ‘id’ parameter is
;; still "name-version" string.
;;
;; Important: as object addresses live only during guile session, elisp
;; part should take care about updating information after "Guix REPL" is
;; restarted (TODO!)
;;
;; ‘installed’ parameter contains information on installed outputs.  It
;; is a list of “installed info”.  Each installed info is alist of
;; installed parameters and their values (see
;; `package-installed-param-alist' and `package-installed-info').

;; To speed-up the process of getting information, the following
;; variables are defined and set on start-up:
;;
;; - `packages' - VHash of "package id"/"package record" pairs.
;;
;; - `packages-table' - Hash table of
;;   "name+version key"/"list of packages" pairs.
;;
;; - `current-manifest-entries-table' - Hash table of
;;   "name+version key"/"list of manifest entries" pairs.  This variable
;;   is set by `set-current-manifest-maybe!' every time manifest is
;;   changed on disk.

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
    (push! module-dir %load-compiled-path)
    (if (and updates-dir (file-exists? updates-dir))
        (begin
          (set! guix-dir updates-dir)
          (push! updates-dir %load-path)
          (push! updates-dir %load-compiled-path))
        (set! guix-dir module-dir))))

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
(define current-manifest-entries-table)
(define packages)
(define packages-table)

(define (name+version->full-name name version)
  (string-append name "-" version))

(define (full-name->name+version full-name)
  (let-values (((name version output)
                (package-specification->name+version+output full-name)))
    (values name version)))

(define name+version->key cons)

(define (key->name+version key)
  (values (car key) (cdr key)))

(define package-id object-address)

(define package-id? integer?)

(define (package-by-id id)
  (and=> (vhash-assq id packages)
         cdr))

(define (set-current-manifest-maybe!)
  (define (manifest-entries->hash-table entries)
    (let ((entries-table (make-hash-table (length entries))))
      (map (lambda (entry)
             (let* ((key (name+version->key
                          (manifest-entry-name entry)
                          (manifest-entry-version entry)))
                    (ref (hash-ref entries-table key)))
               (hash-set! entries-table key
                          (if ref (cons entry ref) (list entry)))))
           entries)
      entries-table))

  (let ((manifest (profile-manifest
                   ;; Is this path always correct?
                   (string-append (getenv "HOME") "/.guix-profile"))))
    (unless (and (manifest? current-manifest)
                 (manifest=? manifest current-manifest))
      (set! current-manifest manifest)
      (set! current-manifest-entries-table
            (manifest-entries->hash-table
             (manifest-entries manifest))))))

(define (set-packages!)
  (let ((count 0))
    (set! packages
          (fold-packages (lambda (pkg res)
                           (set! count (+ 1 count))
                           (vhash-consq (package-id pkg) pkg res))
                         vlist-null))
    (set! packages-table (make-hash-table count))
    (vlist-for-each (lambda (elem)
                      (let* ((pkg (cdr elem))
                             (key (name+version->key
                                   (package-name pkg)
                                   (package-version pkg)))
                             (ref (hash-ref packages-table key)))
                        (hash-set! packages-table key
                                   (if ref (cons pkg ref) (list pkg)))))
                    packages)))

(set-current-manifest-maybe!)
(set-packages!)

(define (manifest-entries-by-name+version name version)
  (or (hash-ref current-manifest-entries-table
                (name+version->key name version))
      '()))

(define (packages-by-name+version name version)
  (or (hash-ref packages-table
                (name+version->key name version))
      '()))

(define (fold-manifest-entries proc init)
  "Fold over `current-manifest-entries-table'.
Call (PROC NAME VERSION ENTRIES RESULT) for each element of the hash
table, using INIT as the initial value of RESULT."
  (hash-fold (lambda (key entries res)
               (let-values (((name version) (key->name+version key)))
                 (proc name version entries res)))
             '()
             current-manifest-entries-table))

(define (fold-object proc init obj)
  (fold proc init
        (if (list? obj) obj (list obj))))

(define (object-info object fields field-alist)
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

(define package-installed-param-alist
  (list
   (cons 'output        manifest-entry-output)
   (cons 'path          manifest-entry-path)
   (cons 'dependencies  manifest-entry-dependencies)))

(define (installed-info-by-manifest-entries entries . params)
  "Return installed info for manifest ENTRIES.

PARAMS are parameter names from `package-installed-param-alist', if
PARAMS are not specified, use all params.

Return list of alists of names and values of installed parameters."
  (map (cut object-info <> params package-installed-param-alist)
       entries))

(define (installed-info-by-name+version name version . params)
  "Return installed info for packages with NAME and VERSION.
For the meaning of PARAMS and returning value, see
`installed-info-by-manifest-entries'"
  (installed-info-by-manifest-entries
   (manifest-entries-by-name+version name version)))

(define (installed-info-by-package package . params)
  "Return installed info for the PACKAGE.
For the meaning of PARAMS and returning value, see
`installed-info-by-manifest-entries'."
  (installed-info-by-name+version (package-name package)
                                  (package-version package)))

(define package-installed-info installed-info-by-package)

(define (package-inputs-names inputs)
  "Return list of full names of the packages from package INPUTS."
  (fold (lambda (input res)
          (let ((pkg (cadr input)))
            (if (package? pkg)
                (cons (package-full-name pkg) res)
                res)))
        '()
        inputs))

(define (package-license-names package)
  "Return list of license names of the PACKAGE."
  (fold-object (lambda (license res)
                 (if (license? license)
                     (cons (license-name license) res)
                     res))
               '()
               (package-license package)))

(define package-param-alist
  (list
   (cons 'id                package-id)
   (cons 'name              package-name)
   (cons 'version           package-version)
   (cons 'license           package-license-names)
   (cons 'synopsis          package-synopsis)
   (cons 'description       package-description)
   (cons 'home-url          package-home-page)
   (cons 'outputs           package-outputs)
   (cons 'inputs            (lambda (pkg) (package-inputs-names
                                      (package-inputs pkg))))
   (cons 'native-inputs     (lambda (pkg) (package-inputs-names
                                      (package-native-inputs pkg))))
   (cons 'propagated-inputs (lambda (pkg) (package-inputs-names
                                      (package-propagated-inputs pkg))))
   (cons 'location          (lambda (pkg) (location->string
                                      (package-location pkg))))
   (cons 'installed         package-installed-info)))

(define (package-param package param)
  "Return the value of a PACKAGE PARAM."
  (define (accessor param)
    (and=> (assq param package-param-alist)
           cdr))
  (and=> (accessor param)
         (cut <> package)))

(define (package-info package . params)
  "Return info about the PACKAGE.

PARAMS are parameter names from `package-param-alist', if PARAMS are not
specified, use all parameters.

Return alist of names and values of package parameters."
  (object-info package params package-param-alist))

(define (matching-packages-info predicate . params)
  "Return list of packages info for the matching packages.
PREDICATE is called on each package.
For the meaning of PARAMS, see `package-info'."
  (fold-packages (lambda (pkg res)
                   (if (predicate pkg)
                       (cons (apply package-info pkg params) res)
                       res))
                 '()))

(define (make-obsolete-info name version entries)
  "Return packages info for obsolete package with NAME and VERSION.
ENTRIES is a list of manifest entries used to get installed info."
  `((id        . ,(name+version->full-name name version))
    (name      . ,name)
    (version   . ,version)
    (outputs   . ,(map manifest-entry-output entries))
    (obsolete  . #t)
    (installed . ,(installed-info-by-manifest-entries
                   entries))))

(define (packages-info-by-name+version name version . params)
  "Return list of packages info for packages with NAME and VERSION.
For the meaning of PARAMS, see `package-info'."
  (let ((packages (packages-by-name+version name version)))
    (if (null? packages)
        (let ((entries (manifest-entries-by-name+version name version)))
          (if (null? entries)
              '()
              (list (make-obsolete-info name version entries))))
        (map (cut apply package-info <> params)
             packages))))

(define (packages-info-by-spec spec . params)
  "Return list of packages info for packages with name specification SPEC.
For the meaning of PARAMS, see `package-info'."
  (set-current-manifest-maybe!)
  (let-values (((name version)
                (full-name->name+version spec)))
    (if version
        (apply packages-info-by-name+version name version params)
        (apply matching-packages-info
               (lambda (pkg)
                 (string=? name (package-name pkg)))
               params))))

(define (packages-info-by-regexp regexp match-params . ret-params)
  "Return list of packages info for packages matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match.
For the meaning of RET-PARAMS, see `package-info'."
  (define (package-match? package regexp)
    (any (lambda (param)
           (let ((val (package-param package param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (set-current-manifest-maybe!)
  (let ((re (make-regexp regexp regexp/icase)))
    (apply matching-packages-info (cut package-match? <> re) ret-params)))

(define (packages-info-by-keys keys . params)
  "Return list of packages info for packages matching KEYS.
KEYS may be an ID, a full-name or a list of these elements."
  (set-current-manifest-maybe!)
  (fold-object
   (lambda (key res)
     (if (package-id? key)
         (let ((pkg (package-by-id key)))
           (if pkg
               (cons (apply package-info pkg params) res)
               res))
         (let-values (((name version)
                       (full-name->name+version key)))
           (let ((info (apply packages-info-by-name+version
                              name version params)))
             (if (null? info)
                 res
                 (append res info))))))
   '()
   keys))

(define (newest-available-packages-info . params)
  "Return list of packages info for the newest available packages.
For the meaning of PARAMS, see `package-info'."
  (set-current-manifest-maybe!)
  (vhash-fold (lambda (key val res)
                (cons (apply package-info (cadr val) params) res))
              '()
              (find-newest-available-packages)))

(define (all-available-packages-info . params)
  "Return list of packages info for all available packages.
For the meaning of PARAMS, see `package-info'."
  (set-current-manifest-maybe!)
  (apply matching-packages-info (lambda (pkg) #t) params))

(define (installed-packages-info . params)
  "Return list of packages info for all installed packages (including obsolete).
For the meaning of PARAMS, see `package-info'."
  (set-current-manifest-maybe!)
  (fold-manifest-entries
   (lambda (name version entries res)
     ;; We don't care about duplicates for the list of
     ;; installed packages, so just take any package (car)
     ;; matching name+version
     (cons (car (packages-info-by-name+version name version)) res))
   '()))

(define (obsolete-packages-info . params)
  "Return list of packages info for obsolete packages.
For the meaning of PARAMS, see `package-info'."
  (set-current-manifest-maybe!)
  (fold-manifest-entries
   (lambda (name version entries res)
     (let ((packages (packages-by-name+version name version)))
       (if (null? packages)
           (cons (make-obsolete-info name version entries) res)
           res)))
   '()))

