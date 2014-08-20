;;; guix-main.scm --- Main scheme code for guix.el package

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

;; Information about packages and generations is passed to the elisp
;; side in the form of alists of parameters (such as ‘name’ or
;; ‘version’) and their values.  These alists are called "entries" in
;; this program.  So to distinguish, just "package" in the name of a
;; function means a guile object ("package" record) while
;; "package entry" means alist of package parameters and values (see
;; ‘package->package-entry’ and ‘package-param-alist’).
;;
;; "Entry" is probably not the best name for such alists, because there
;; already exists "manifest-entry" which has nothing to do with the
;; "entry" described above.  Do not be confused :)

;; The following functions may be called to get information about the
;; packages (returning value is a list of package entries):
;;
;; - package-entries-by-keys
;; - package-entries-by-spec
;; - package-entries-by-regexp
;; - all-available-package-entries
;; - newest-available-package-entries
;; - installed-package-entries
;; - obsolete-package-entries

;; Since name/version pair is not necessarily unique, we use
;; `object-address' to identify a package (for ‘id’ parameter), if
;; possible.  However for the obsolete packages (that can be found in
;; installed manifest but not in a package directory), ‘id’ parameter is
;; still "name-version" string.  So ‘id’ package parameter in the code
;; below is either an object-address number or a full-name string.
;;
;; Important: as object addresses live only during guile session, elisp
;; part should take care about updating information after "Guix REPL" is
;; restarted (TODO!)
;;
;; ‘installed’ parameter of a package entry contains information about
;; installed outputs.  It is a list of "installed entries" (see
;; ‘package-installed-param-alist’).

;; To speed-up the process of getting information, the following
;; variables are defined and set on start-up:
;;
;; - `packages' - VHash of "package address"/"package" pairs.
;;
;; - `packages-table' - Hash table of
;;   "name+version key"/"list of packages" pairs.
;;
;; - `current-manifest-entries-table' - Hash table of
;;   "name+version key"/"list of manifest entries" pairs.  This variable
;;   is set by `set-current-manifest-maybe!' every time manifest is
;;   changed on disk.

;;; Code:

(use-modules
 (ice-9 vlist)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-11)
 (srfi srfi-19)
 (srfi srfi-26)
 (guix)
 (guix config)
 (guix packages)
 (guix profiles)
 (guix licenses)
 (guix utils)
 (guix ui)
 (guix scripts package)
 (gnu packages))

(define %user-profile
  (and=> (getenv "HOME")
         (cut string-append <> "/.guix-profile")))

(define %current-profile
  (string-append %state-directory "/profiles/"
                 (or (and=> (getenv "USER")
                            (cut string-append "per-user/" <>))
                     "default")
                 "/guix-profile"))

(define %current-manifest)
(define current-manifest-entries-table)
(define packages)
(define packages-table)

(define-syntax-rule (first-or-false lst)
  (and (not (null? lst))
       (first lst)))

(define full-name->name+version package-name->name+version)
(define (name+version->full-name name version)
  (string-append name "-" version))

(define* (make-package-specification name #:optional version output)
  (let ((full-name (if version
                       (name+version->full-name name version)
                       name)))
    (if output
        (string-append full-name ":" output)
        full-name)))

(define name+version->key cons)
(define (key->name+version key)
  (values (car key) (cdr key)))

(define* (set-current-manifest-maybe! #:optional manifest)
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

  (let ((manifest (or manifest (profile-manifest %user-profile))))
    (unless (and (manifest? %current-manifest)
                 (equal? manifest %current-manifest))
      (set! %current-manifest manifest)
      (set! current-manifest-entries-table
            (manifest-entries->hash-table
             (manifest-entries manifest))))))

(define (set-packages!)
  (let ((count 0))
    (set! packages
          (fold-packages (lambda (pkg res)
                           (set! count (+ 1 count))
                           (vhash-consq (object-address pkg) pkg res))
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

(set-packages!)

(define (manifest-entries-by-name+version name version)
  (or (hash-ref current-manifest-entries-table
                (name+version->key name version))
      '()))

(define (packages-by-name+version name version)
  (or (hash-ref packages-table
                (name+version->key name version))
      '()))

(define (packages-by-full-name full-name)
  (call-with-values
      (lambda () (full-name->name+version full-name))
    packages-by-name+version))

(define (package-by-address address)
  (and=> (vhash-assq address packages)
         cdr))

(define (packages-by-id id)
  (if (integer? id)
      (let ((pkg (package-by-address id)))
        (if pkg (list pkg) '()))
      (packages-by-full-name id)))

(define (package-by-id id)
  (first-or-false (packages-by-id id)))

(define (newest-package-by-id id)
  (and=> (id->name+version id)
         (lambda (name)
           (first-or-false (find-best-packages-by-name name #f)))))

(define (id->name+version id)
  (if (integer? id)
      (and=> (package-by-address id)
             (lambda (pkg)
               (values (package-name pkg)
                       (package-version pkg))))
      (full-name->name+version id)))

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

(define* (object-info object param-alist #:optional (params '()))
  "Return info about the OBJECT.

PARAM-ALIST is alist of available OBJECT parameters (symbols) and
functions returning values of these parameters.  Each function is called
with OBJECT as a single argument.

PARAMS is list of returned parameters from PARAM-ALIST.  If PARAMS is
not specified or is an empty list, use all available parameters.

For each parameter from PARAMS a corresponding function is called on
OBJECT.  Returning value is alist of PARAMS and the values of funcalls."
  (let ((use-all-params (null? params)))
    (filter-map (match-lambda
                 ((param . fun)
                  (and (or use-all-params
                           (memq param params))
                       (cons param (fun object))))
                 (_ #f))
                param-alist)))

(define package-installed-param-alist
  (list
   (cons 'output       manifest-entry-output)
   (cons 'path         manifest-entry-item)
   (cons 'dependencies manifest-entry-dependencies)))

(define (manifest-entry->installed-entry entry . params)
  "Return installed entry info for manifest ENTRY."
  (object-info entry package-installed-param-alist params))

(define (manifest-entries->installed-entries entries . params)
  "Return list of installed entries for manifest ENTRIES."
  (map (cut apply manifest-entry->installed-entry <> params)
       entries))

(define (installed-entries-by-name+version name version . params)
  "Return list of installed entries for packages with NAME and VERSION."
  (apply manifest-entries->installed-entries
         (manifest-entries-by-name+version name version)
         params))

(define (installed-entries-by-package package . params)
  "Return list of installed entries for the PACKAGE."
  (apply installed-entries-by-name+version
         (package-name package)
         (package-version package)
         params))

(define (package-inputs-names inputs)
  "Return list of full names of the packages from package INPUTS."
  (filter-map (match-lambda
               ((_ (? package? package))
                (package-full-name package))
               (_ #f))
              inputs))

(define (package-license-names package)
  "Return list of license names of the PACKAGE."
  (fold-object (lambda (license res)
                 (if (license? license)
                     (cons (license-name license) res)
                     res))
               '()
               (package-license package)))

(define (package-unique? package)
  "Return #t if PACKAGE is a single package with such name/version."
  (null? (cdr (packages-by-name+version (package-name package)
                                        (package-version package)))))

(define package-param-alist
  (list
   (cons 'id                object-address)
   (cons 'name              package-name)
   (cons 'version           package-version)
   (cons 'license           package-license-names)
   (cons 'synopsis          package-synopsis)
   (cons 'description       package-description)
   (cons 'home-url          package-home-page)
   (cons 'outputs           package-outputs)
   (cons 'non-unique        (negate package-unique?))
   (cons 'inputs            (lambda (pkg) (package-inputs-names
                                      (package-inputs pkg))))
   (cons 'native-inputs     (lambda (pkg) (package-inputs-names
                                      (package-native-inputs pkg))))
   (cons 'propagated-inputs (lambda (pkg) (package-inputs-names
                                      (package-propagated-inputs pkg))))
   (cons 'location          (lambda (pkg) (location->string
                                      (package-location pkg))))
   (cons 'installed         installed-entries-by-package)))

(define (package-param package param)
  "Return the value of a PACKAGE PARAM."
  (define (accessor param)
    (and=> (assq param package-param-alist)
           cdr))
  (and=> (accessor param)
         (cut <> package)))

(define (package->package-entry package . params)
  "Return info about the PACKAGE.

PARAMS are parameter names from `package-param-alist', if PARAMS are not
specified, use all parameters.

Return alist of names and values of package parameters."
  (object-info package package-param-alist params))

(define (matching-package-entries predicate . params)
  "Return list of package entries for the matching packages.
PREDICATE is called on each package."
  (fold-packages (lambda (pkg res)
                   (if (predicate pkg)
                       (cons (apply package->package-entry pkg params)
                             res)
                       res))
                 '()))

(define (make-obsolete-package-entry name version entries)
  "Return package entry for an obsolete package with NAME and VERSION.
ENTRIES is a list of manifest entries used to get installed info."
  `((id        . ,(name+version->full-name name version))
    (name      . ,name)
    (version   . ,version)
    (outputs   . ,(map manifest-entry-output entries))
    (obsolete  . #t)
    (installed . ,(manifest-entries->installed-entries entries))))

(define (package-entries-by-name+version name version . params)
  "Return list of package entries for packages with NAME and VERSION."
  (let ((packages (packages-by-name+version name version)))
    (if (null? packages)
        (let ((entries (manifest-entries-by-name+version name version)))
          (if (null? entries)
              '()
              (list (make-obsolete-package-entry name version entries))))
        (map (cut apply package->package-entry <> params)
             packages))))

(define (package-entries-by-spec spec . params)
  "Return list of package entries for packages with name specification SPEC."
  (set-current-manifest-maybe!)
  (let-values (((name version)
                (full-name->name+version spec)))
    (if version
        (apply package-entries-by-name+version name version params)
        (apply matching-package-entries
               (lambda (pkg)
                 (string=? name (package-name pkg)))
               params))))

(define (package-entries-by-regexp regexp match-params . ret-params)
  "Return list of package entries for packages matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match."
  (define (package-match? package regexp)
    (any (lambda (param)
           (let ((val (package-param package param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (set-current-manifest-maybe!)
  (let ((re (make-regexp regexp regexp/icase)))
    (apply matching-package-entries
           (cut package-match? <> re)
           ret-params)))

(define (package-entries-by-ids ids . params)
  "Return list of package entries for packages matching KEYS.
IDS may be an object-address, a full-name or a list of such elements."
  (set-current-manifest-maybe!)
  (fold-object
   (lambda (id res)
     (if (integer? id)
         (let ((pkg (package-by-address id)))
           (if pkg
               (cons (apply package->package-entry pkg params) res)
               res))
         (let ((entries (apply package-entries-by-spec id params)))
           (if (null? entries)
               res
               (append res entries)))))
   '()
   ids))

(define (newest-available-package-entries . params)
  "Return list of package entries for the newest available packages."
  (set-current-manifest-maybe!)
  (vhash-fold (lambda (key val res)
                (cons (apply package->package-entry (cadr val) params)
                      res))
              '()
              (find-newest-available-packages)))

(define (all-available-package-entries . params)
  "Return list of package entries for all available packages."
  (set-current-manifest-maybe!)
  (apply matching-package-entries (const #t) params))

(define (manifest-package-entries . params)
  "Return list of package entries for the current manifest."
  (fold-manifest-entries
   (lambda (name version entries res)
     ;; We don't care about duplicates for the list of
     ;; installed packages, so just take any package (car)
     ;; matching name+version
     (cons (car (apply package-entries-by-name+version
                       name version params))
           res))
   '()))

(define (installed-package-entries . params)
  "Return list of package entries for all installed packages."
  (set-current-manifest-maybe!)
  (apply manifest-package-entries params))

(define (generation-package-entries generation . params)
  "Return list of package entries for packages from GENERATION."
  (set-current-manifest-maybe!
   (profile-manifest
    (generation-file-name %current-profile generation)))
  (apply manifest-package-entries params))

(define (obsolete-package-entries . params)
  "Return list of package entries for obsolete packages."
  (set-current-manifest-maybe!)
  (fold-manifest-entries
   (lambda (name version entries res)
     (let ((packages (packages-by-name+version name version)))
       (if (null? packages)
           (cons (make-obsolete-package-entry name version entries) res)
           res)))
   '()))


;;; Generation entries

(define* (profile-generations #:optional (profile %current-profile))
  "Return list of generations for PROFILE."
  (let ((generations (generation-numbers profile)))
    (if (equal? generations '(0))
        '()
        generations)))

(define* (generation-param-alist #:optional (profile %current-profile))
  "Return alist of generation parameters and functions for PROFILE."
  (list
   (cons 'id          identity)
   (cons 'number      identity)
   (cons 'prev-number (cut previous-generation-number profile <>))
   (cons 'path        (cut generation-file-name profile <>))
   (cons 'time        (lambda (gen)
                        (time-second (generation-time profile gen))))))

(define (generation->generation-entry generation . params)
  "Return generation entry info for the GENERATION."
  (object-info generation (generation-param-alist) params))

(define (generations->generation-entries generations . params)
  "Return list of generation entries for GENERATIONS."
  (map (cut apply generation->generation-entry <> params)
       generations))

(define (matching-generation-entries predicate . params)
  "Return list of generation entries for the matching generations.
PREDICATE is called on each generation."
  (fold (lambda (gen res)
          (if (predicate gen)
              (cons (apply generation->generation-entry gen params)
                    res)
              res))
        '()
        (profile-generations)))

(define (all-generation-entries . params)
  "Return list of all generation entries."
  (set-current-manifest-maybe!)
  (apply generations->generation-entries
         (profile-generations)
         params))

(define (last-generation-entries number . params)
  "Return list of last NUMBER generation entries.
If NUMBER is 0 or less, return all generation entries."
  (set-current-manifest-maybe!)
  (let ((generations (profile-generations))
        (number (if (<= number 0) +inf.0 number)))
    (apply generations->generation-entries
           (if (> (length generations) number)
               (list-head  (reverse generations) number)
               generations)
           params)))

(define (generation-entries-by-ids ids . params)
  "Return list of generation entries for generations matching IDS.
IDS is a list of generation numbers."
  (set-current-manifest-maybe!)
  (apply matching-generation-entries
         (cut memq <> ids)
         params))


;;; Actions

(define* (package->manifest-entry* package #:optional output)
  (and package
       (begin
         (check-package-freshness package)
         (package->manifest-entry package output))))

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

(define* (process-package-actions #:key (install '()) (upgrade '()) (remove '())
                                  (use-substitutes? #t) dry-run?)
  "Perform package actions.

INSTALL, UPGRADE, REMOVE are lists of 'package action patterns'.
Each pattern should have the following form:

  (ID . OUTPUTS)

ID is an object address or a full-name of a package.
OUTPUTS is a list of package outputs (may be an empty list)."
  (format #t "The process begins ...~%")
  (set-current-manifest-maybe!)
  (let* ((install (append
                   (convert-action-patterns
                    install make-install-manifest-entries)
                   (convert-action-patterns
                    upgrade make-upgrade-manifest-entries)))
         (remove (convert-action-patterns remove make-manifest-pattern))
         (transaction (manifest-transaction (install install)
                                            (remove remove)))
         (new-manifest (manifest-perform-transaction
                        %current-manifest transaction)))
    (unless (and (null? install) (null? remove))
      (let* ((store (open-connection))
             (derivation (run-with-store
                          store (profile-derivation new-manifest)))
             (derivations (list derivation))
             (new-profile (derivation->output-path derivation)))
        (set-build-options store
                           #:use-substitutes? use-substitutes?)
        (manifest-show-transaction store %current-manifest transaction
                                   #:dry-run? dry-run?)
        (show-what-to-build store derivations
                            #:use-substitutes? use-substitutes?
                            #:dry-run? dry-run?)
        (unless dry-run?
          (let ((name (generation-file-name
                       %current-profile
                       (+ 1 (generation-number %current-profile)))))
            (and (build-derivations store derivations)
                 (let* ((entries (manifest-entries new-manifest))
                        (count   (length entries)))
                   (switch-symlinks name new-profile)
                   (switch-symlinks %current-profile name)
                   (format #t (N_ "~a package in profile~%"
                                  "~a packages in profile~%"
                                  count)
                           count)))))))))

