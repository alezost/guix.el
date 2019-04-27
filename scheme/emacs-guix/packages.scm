;;; packages.scm --- Guix packages and generations

;; Copyright © 2014–2019 Alex Kost <alezost@gmail.com>

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

;; This module provides the code to find packages and generations.

;; Information about packages and generations is passed to the elisp
;; side in the form of alists of parameters (such as 'name' or
;; 'version') and their values.

;; 'sexps' procedure is the "entry point" for the elisp side to get
;; information about packages and generations.

;; Since name/version pair is not necessarily unique, we use
;; 'object-address' to identify a package (for 'id' parameter), if
;; possible.  However for the unknown packages (that can be found in
;; installed manifest but not in a package directory), 'id' parameter is
;; still "name-version" string.  So 'id' package parameter in the code
;; below is either an object-address number or a full-name string.

;;; Code:

(define-module (emacs-guix packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (guix graph)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix scripts graph)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:autoload   (gnu system) (operating-system-packages)
  #:autoload   (guix scripts system) (read-operating-system)
  #:autoload   (guix git-download) (git-reference?
                                    git-reference-url)
  #:autoload   (guix licenses) (license?
                                license-name)
  #:autoload   (emacs-guix licenses) (lookup-license)
  #:use-module (emacs-guix emacs)
  #:use-module (emacs-guix profiles)
  #:use-module (emacs-guix utils)
  #:export (package-names
            package-names*
            number-of-packages
            register-package
            package-specification
            profile->specifications+file-names
            id->name+version
            package-by-id
            package-by-id-or-name
            packages-by-id
            newest-package-by-id
            packages-by-name
            packages-by-full-name
            packages-by-regexp
            packages-by-license
            all-packages
            packages-from-file
            matching-packages
            package/output-sexps
            package-location-string
            package-location-files
            package-location-sexps))

(define (full-name->name+version spec)
  "Given package specification SPEC with or without output,
return two values: name and version.  For example, for SPEC
\"foo@0.9.1b:lib\", return \"foo\" and \"0.9.1b\"."
  (let-values (((name version output)
                (package-specification->name+version+output spec)))
    (values name version)))

(define (name+version->full-name name version)
  (string-append name "@" version))

(define* (make-package-specification name #:optional version output)
  "Convert NAME, VERSION and OUTPUT to a package specification string."
  (let ((full-name (if version
                       (name+version->full-name name version)
                       name)))
    (if output
        (string-append full-name ":" output)
        full-name)))

(define* (package-specification package #:optional output)
  "Convert PACKAGE object to a package specification string."
  (make-package-specification (package-name package)
                              (package-version package)
                              output))

(define (manifest-entry-package-specification entry)
  (call-with-values
      (lambda () (manifest-entry->name+version+output entry))
    make-package-specification))

(define (manifest-entries-package-specifications entries)
  (map manifest-entry-package-specification entries))

(define (profile-package-specifications profile)
  "Return a list of package specifications for PROFILE."
  (let ((manifest (profile-manifest profile)))
    (manifest-entries-package-specifications
     (manifest-entries manifest))))

(define (profile->specifications+file-names profile)
  "Return a list of package specifications and file names for PROFILE.
Each element of the list is a list of the package specification and its
file name."
  (let ((manifest (profile-manifest profile)))
    (map (lambda (entry)
           (list (manifest-entry-package-specification entry)
                 (manifest-entry-item entry)))
         (manifest-entries manifest))))

(define (profile-difference profile1 profile2)
  "Return a list of package specifications for outputs installed in PROFILE1
and not installed in PROFILE2."
  (let ((specs1 (profile-package-specifications profile1))
        (specs2 (profile-package-specifications profile2)))
    (lset-difference string=? specs1 specs2)))

(define %manifest-entry-param-alist
  `((output       . ,manifest-entry-output)
    (file-name    . ,manifest-entry-item)
    (dependencies . ,manifest-entry-dependencies-file-names)))

(define manifest-entry->sexp
  (object-transformer %manifest-entry-param-alist))

(define (manifest-entries->sexps entries)
  (map manifest-entry->sexp entries))

(define (package-inputs-sexps inputs)
  "Return a list of full names of the packages from package INPUTS."
  (filter-map (match-lambda
                ((_ (? package? package))
                 (list (object-address package)
                       (package-specification package)))
                ((_ (? package? package) output)
                 (list (object-address package)
                       (package-specification package output)))
                (_ #f))
              inputs))

(define (package-license-names package)
  "Return a list of license names of the PACKAGE."
  (filter-map (lambda (license)
                (and (license? license)
                     (license-name license)))
              (list-maybe (package-license package))))

(define (package-source-names package)
  "Return a list of source names (URLs) of the PACKAGE."
  (let ((source (package-source package)))
    (and (origin? source)
         (filter-map (lambda (uri)
                       (cond ((string? uri)
                              uri)
                             ((git-reference? uri)
                              (git-reference-url uri))
                             (else "Unknown source type")))
                     (list-maybe (origin-uri source))))))

(define (package-unique? package)
  "Return #t if PACKAGE is a single package with such name/version."
  (match (packages-by-name (package-name package)
                           (package-version package))
    ((first rest ..1) #f)
    ;; Empty list also leads to #t: if no packages have been found, then
    ;; PACKAGE (probably the hidden one) is also considered unique.
    (_ #t)))

(define (package-known-status name version)
  "Return 'known status' of a package with NAME and VERSION.
The result is one of the following symbols:

  'known' - the package exists;
  'unknown' - the package does not exist;
  'obsolete' - the package NAME exists but has a newer version;
  'future' - the package NAME exists but has an older version."
  (let ((package (first-or-false (find-best-packages-by-name name #f))))
    (if package
        (case (version-compare version (package-version package))
          ((<) 'obsolete)
          ((>) 'future)
          (else 'known))
        'unknown)))

(define %package-param-alist
  `((id                . ,object-address)
    (package-id        . ,object-address)
    (name              . ,package-name)
    (upstream-name     . ,package-upstream-name)
    (version           . ,package-version)
    (license           . ,package-license-names)
    (source            . ,package-source-names)
    (synopsis          . ,package-synopsis-string)
    (description       . ,package-description-string)
    (home-url          . ,package-home-page)
    (outputs           . ,package-outputs)
    (systems           . ,package-supported-systems)
    (non-unique        . ,(negate package-unique?))
    (hidden            . ,hidden-package?)
    (known-status      . ,(const 'known))
    (superseded        . ,(lambda (pkg)
                            (and=> (package-superseded pkg)
                                   package-specification)))
    (inputs            . ,(compose package-inputs-sexps
                                   package-inputs))
    (native-inputs     . ,(compose package-inputs-sexps
                                   package-native-inputs))
    (propagated-inputs . ,(compose package-inputs-sexps
                                   package-propagated-inputs))
    (location          . ,(compose location->string
                                   package-location))))

(define (package-param package param)
  "Return a value of a PACKAGE PARAM."
  (and=> (assq-ref %package-param-alist param)
         (cut <> package)))


;;; Finding packages

(define-values (package-by-address
                register-package)
  (let ((table (delay (fold-packages
                       (lambda (package table)
                         (vhash-consq (object-address package)
                                      package table))
                       vlist-null
                       #:select? (const #t)))))
    (values
     (lambda (address)
       "Return package by its object ADDRESS."
       (match (vhash-assq address (force table))
         ((_ . package) package)
         (_ #f)))
     (lambda (package)
       "Register PACKAGE by its 'object-address', so that later
'package-by-address' can be used to access it."
       (let ((table* (force table)))
         (set! table
               (delay (vhash-consq (object-address package)
                                   package table*))))))))

(define packages-by-name+version
  (let ((table (delay (fold-packages
                       (lambda (package table)
                         (vhash-cons (cons (package-name package)
                                           (package-version package))
                                     package table))
                       vlist-null))))
    (lambda (name version)
      "Return packages matching NAME and VERSION."
      (vhash-fold* cons '() (cons name version) (force table)))))

(define (packages-by-full-name full-name)
  (call-with-values
      (lambda () (full-name->name+version full-name))
    packages-by-name+version))

(define (packages-by-id id)
  (if (integer? id)
      (let ((pkg (package-by-address id)))
        (if pkg (list pkg) '()))
      (packages-by-full-name id)))

(define (id->name+version id)
  (if (integer? id)
      (and=> (package-by-address id)
             (lambda (pkg)
               (values (package-name pkg)
                       (package-version pkg))))
      (full-name->name+version id)))

(define (package-by-id id)
  (first-or-false (packages-by-id id)))

(define (package-by-id-or-name id-or-name)
  "Return package object by ID-OR-NAME.
ID-OR-NAME may be either a package ID (object address) or its name."
  (or (package-by-id id-or-name)
      (and (string? id-or-name)
           (match (packages-by-name id-or-name)
             (()              #f)
             ((package _ ...) package)))))

(define (newest-package-by-id id)
  (and=> (id->name+version id)
         (lambda (name)
           (first-or-false (find-best-packages-by-name name #f)))))

(define (objects->packages objects)
  "Convert OBJECTS to packages.
OBJECTS can be either package IDs, specifications or the packages
themselves."
  (filter-map (match-lambda
                ((? package? package)
                 package)
                (id-or-name
                 (package-by-id-or-name id-or-name)))
              objects))

(define* (matching-packages predicate
                            #:key (select? (negate hidden-package?)))
  "Return all packages matching PREDICATE.
If (PREDICATE package) returns #f, it is not a matching package,
otherwise, it is.
See `fold-packages' for the meaning of SELECT?."
  (fold-packages (lambda (pkg res)
                   (if (predicate pkg)
                       (cons pkg res)
                       res))
                 '()
                 #:select? select?))

(define (filter-packages-by-output packages output)
  (filter (lambda (package)
            (member output (package-outputs package)))
          packages))

(define* (packages-by-name name #:optional version output)
  "Return a list of packages matching NAME, VERSION and OUTPUT."
  (let ((packages (if version
                      (packages-by-name+version name version)
                      (matching-packages
                       (lambda (pkg) (string=? name (package-name pkg)))))))
    (if output
        (filter-packages-by-output packages output)
        packages)))

(define (manifest-entry->packages entry)
  (call-with-values
      (lambda () (manifest-entry->name+version+output entry))
    packages-by-name))

(define (packages-by-regexp regexp match-params)
  "Return a list of packages matching REGEXP string.
MATCH-PARAMS is a list of parameters that REGEXP can match."
  (define (package-match? package regexp)
    (any (lambda (param)
           (let ((val (package-param package param)))
             (and (string? val) (regexp-exec regexp val))))
         match-params))

  (let ((re (make-regexp regexp regexp/icase)))
    (matching-packages (cut package-match? <> re))))

(define (packages-by-license license)
  "Return a list of packages with LICENSE."
  (matching-packages
   (lambda (package)
     (memq license (list-maybe (package-license package))))))

(define (hidden-packages)
  "Return a list of hidden packages."
  (matching-packages hidden-package?
                     #:select? (const #t)))

(define (superseded-packages)
  "Return a list of superseded packages."
  (matching-packages package-superseded))

(define (all-packages)
  "Return a list of all available packages."
  (matching-packages (const #t)))

(define (packages-from-file file)
  "Return a list of packages from FILE."
  (let ((package (load (canonicalize-path file))))
    (if (package? package)
        (begin
          (register-package package)
          (list package))
        '())))

(define (packages-from-system-config-file file)
  "Return a list of packages from system configuration FILE."
  (operating-system-packages (read-operating-system file)))

(define* (dependent-packages packages #:key direct?)
  ;; Some code for this procedure was taken from `list-dependents'
  ;; procedure in (guix scripts refresh) module.
  "Return a list of packages that need to be rebuilt if PACKAGES are changed."
  (define (all-packages)
    (matching-packages (const #t) #:select? (const #t)))

  (let ((packages (objects->packages packages)))
    (with-store store
      (run-with-store store
        (mlet %store-monad ((edges (node-back-edges %bag-node-type
                                                    (all-packages))))
          (if direct?
              (return (append-map edges packages))
              (return (node-transitive-edges packages edges))))))))


;;; Making package/output patterns.

(define (specification->package-pattern specification)
  (call-with-values
      (lambda ()
        (full-name->name+version specification))
    list))

(define (specification->output-pattern specification)
  (call-with-values
      (lambda ()
        (package-specification->name+version+output specification #f))
    list))

(define (id->package-pattern id-or-spec)
  "Return a package pattern by ID-OR-SPEC.
Package pattern is either a package or (name version) list.
ID-OR-SPEC should be a package address or a package specification
or (ADDRESS SPEC) list."
  (match id-or-spec
    ((id spec)
     (or (package-by-address id)
         (specification->package-pattern spec)))
    ((? integer? id)
     (package-by-address id))
    (_ (specification->package-pattern id-or-spec))))

(define (id->output-pattern id-or-spec)
  "Return an output pattern by output ID-OR-SPEC.
Output pattern is either (package output) or (name version output) list.
ID-OR-SPEC should be '<package-address>:<output>' string or
'<name>-<version>:<output>' string or (ADDRESS SPEC) list."
  (match id-or-spec
    ((id spec)
     (or (list (package-by-address id)
               (let-values (((_name _version output)
                             (package-specification->name+version+output
                              spec)))
                 output))
         (specification->output-pattern spec)))
    ((? integer? id)
     (list (package-by-address id) "out"))
    (_
     (let-values (((name version output)
                   (package-specification->name+version+output id-or-spec)))
       (if version
           (list name version output)
           (list (package-by-address (string->number name))
                 output))))))

(define (specifications->package-patterns . specifications)
  (map specification->package-pattern specifications))

(define (specifications->output-patterns . specifications)
  (map specification->output-pattern specifications))

(define (ids->package-patterns . ids)
  (map id->package-pattern ids))

(define (ids->output-patterns . ids)
  (map id->output-pattern ids))

(define* (manifest-patterns-result packages res unknown-pattern
                                   #:optional installed-pattern)
  "Auxiliary procedure for 'manifest-package-patterns' and
'manifest-output-patterns'."
  (if (null? packages)
      (cons (unknown-pattern) res)
      (if installed-pattern
          ;; We don't need duplicates for a list of installed packages,
          ;; so just take any (car) package.
          (cons (installed-pattern (car packages)) res)
          res)))

(define* (manifest-package-patterns manifest #:optional unknown-only?)
  "Return a list of package patterns for MANIFEST entries.
If UNKNOWN-ONLY? is #f, use all entries, otherwise make patterns only
for unknown packages."
  (fold-manifest-by-name
   manifest
   (lambda (name version entries res)
     (manifest-patterns-result (packages-by-name name version)
                               res
                               (lambda () (list name version entries))
                               (and (not unknown-only?)
                                    (cut list <> entries))))
   '()))

(define* (manifest-output-patterns manifest #:optional unknown-only?)
  "Return a list of output patterns for MANIFEST entries.
If UNKNOWN-ONLY? is #f, use all entries, otherwise make patterns only
for unknown packages."
  (fold (lambda (entry res)
          (manifest-patterns-result (manifest-entry->packages entry)
                                    res
                                    (lambda () entry)
                                    (and (not unknown-only?)
                                         (cut list <> entry))))
        '()
        (manifest-entries manifest)))

(define (unknown-package-patterns manifest)
  (manifest-package-patterns manifest #t))

(define (unknown-output-patterns manifest)
  (manifest-output-patterns manifest #t))


;;; Transforming package/output patterns into alists.

(define (unknown-package-sexp name version entries)
  "Return an alist with information about unknown package.
ENTRIES is a list of installed manifest entries."
  (define default
    `((id        . ,(name+version->full-name name version))
      (name      . ,name)
      (version   . ,version)
      (outputs   . ,(map manifest-entry-output entries))
      (installed . ,(manifest-entries->sexps entries))))

  ;; If it is 'guix' with commit-like version, then it sure is the
  ;; package from "guix pull"-ed profile, isn't it?
  (if (and (string=? name "guix")
           (not (string-match "\\." version)))
      (append
       '((known-status . pull))
       default
       (or (and-let* ((props (manifest-entry-properties (car entries)))
                      (source-sexp (assq 'source props)))
             (cdr source-sexp))
           '()))
      (cons (cons 'known-status (package-known-status name version))
            default)))

(define (package-pattern-transformer manifest params)
  "Return 'package-pattern->package-sexps' procedure."
  (define package->sexp
    (object-transformer %package-param-alist params))

  (define* (sexp-by-package package #:optional
                            (entries (manifest-entries-by-name
                                      manifest
                                      (package-name package)
                                      (package-version package))))
    (cons (cons 'installed (manifest-entries->sexps entries))
          (package->sexp package)))

  (define (->sexps pattern)
    (match pattern
      ((? package? package)
       (list (sexp-by-package package)))
      (((? package? package) entries)
       (list (sexp-by-package package entries)))
      ((name version entries)
       (list (unknown-package-sexp
              name version entries)))
      ((name version)
       (let ((packages (packages-by-name name version)))
         (if (null? packages)
             (let ((entries (manifest-entries-by-name
                             manifest name version)))
               (if (null? entries)
                   '()
                   (list (unknown-package-sexp
                          name version entries))))
             (map sexp-by-package packages))))
      (_ '())))

  ->sexps)

(define (output-pattern-transformer manifest params)
  "Return 'output-pattern->output-sexps' procedure."
  (define package->sexp
    (object-transformer (alist-delete 'id %package-param-alist)
                        params))

  (define manifest-entry->sexp
    (object-transformer (alist-delete 'output %manifest-entry-param-alist)
                        params))

  (define* (output-sexp pkg-alist pkg-address output
                        #:optional entry)
    (let ((entry-alist (if entry
                           (manifest-entry->sexp entry)
                           '()))
          (base `((id        . ,(string-append
                                 (number->string pkg-address)
                                 ":" output))
                  (output    . ,output)
                  (installed . ,(->bool entry)))))
      (append entry-alist base pkg-alist)))

  (define (unknown-output-sexp entry)
    (let-values (((name version output)
                  (manifest-entry->name+version+output entry)))
      (let ((base `((id         . ,(make-package-specification
                                    name version output))
                    (package-id . ,(name+version->full-name name version))
                    (name       . ,name)
                    (version    . ,version)
                    (output     . ,output)
                    (known-status . ,(package-known-status name version))
                    (installed  . #t))))
        (append (manifest-entry->sexp entry) base))))

  (define* (sexps-by-package package #:optional output
                             (entries (manifest-entries-by-name
                                       manifest
                                       (package-name package)
                                       (package-version package))))
    ;; Assuming that PACKAGE has this OUTPUT.
    (let ((pkg-alist (package->sexp package))
          (address (object-address package))
          (outputs (if output
                       (list output)
                       (package-outputs package))))
      (map (lambda (output)
             (output-sexp pkg-alist address output
                          (manifest-entry-by-output entries output)))
           outputs)))

  (define* (sexps-by-manifest-entry entry #:optional
                                    (packages (manifest-entry->packages
                                               entry)))
    (if (null? packages)
        (list (unknown-output-sexp entry))
        (map (lambda (package)
               (output-sexp (package->sexp package)
                            (object-address package)
                            (manifest-entry-output entry)
                            entry))
             packages)))

  (define (->sexps pattern)
    (match pattern
      ((? package? package)
       (sexps-by-package package))
      ((package (? string? output))
       (sexps-by-package package output))
      ((? manifest-entry? entry)
       (list (unknown-output-sexp entry)))
      ((package entry)
       (sexps-by-manifest-entry entry (list package)))
      ((name version output)
       (let ((packages (packages-by-name name version output)))
         (if (null? packages)
             (let ((entries (manifest-entries-by-name
                             manifest name version output)))
               (append-map (cut sexps-by-manifest-entry <>)
                           entries))
             (append-map (cut sexps-by-package <> output)
                         packages))))
      (_ '())))

  ->sexps)

(define (entry-type-error entry-type)
  (error (format #f "Wrong entry-type '~a'" entry-type)))

(define (search-type-error entry-type search-type)
  (error (format #f "Wrong search type '~a' for entry-type '~a'"
                 search-type entry-type)))

(define %pattern-transformers
  `((package . ,package-pattern-transformer)
    (output  . ,output-pattern-transformer)))

(define (pattern-transformer entry-type)
  (assq-ref %pattern-transformers entry-type))

;; All procedures from inner alists are called with (MANIFEST . SEARCH-VALUES)
;; as arguments; see `package/output-sexps'.
(define %patterns-makers
  (let* ((apply-to-rest         (lambda (proc)
                                  (lambda (_ . rest) (apply proc rest))))
         (apply-to-first        (lambda (proc)
                                  (lambda (first . _) (proc first))))
         (manifest-package-proc (apply-to-first manifest-package-patterns))
         (manifest-output-proc  (apply-to-first manifest-output-patterns))
         (regexp-proc           (lambda (_ regexp params . __)
                                  (packages-by-regexp regexp params)))
         (license-proc          (lambda (_ license-name)
                                  (packages-by-license
                                   (lookup-license license-name))))
         (location-proc         (lambda (_ location)
                                  (packages-by-location-file location)))
         (file-proc             (lambda (_ file)
                                  (packages-from-file file)))
         (os-file-proc          (lambda (_ file)
                                  (packages-from-system-config-file file)))
         (dependent-proc        (lambda (_ type packages)
                                  (dependent-packages
                                   packages
                                   #:direct? (eq? type 'direct))))
         (hidden-proc           (lambda _ (hidden-packages)))
         (superseded-proc       (lambda _ (superseded-packages)))
         (all-proc              (lambda _ (all-packages))))
    `((package
       (id               . ,(apply-to-rest ids->package-patterns))
       (name             . ,(apply-to-rest specifications->package-patterns))
       (installed        . ,manifest-package-proc)
       (unknown          . ,(apply-to-first unknown-package-patterns))
       (regexp           . ,regexp-proc)
       (license          . ,license-proc)
       (location         . ,location-proc)
       (from-file        . ,file-proc)
       (from-os-file     . ,os-file-proc)
       (hidden           . ,hidden-proc)
       (superseded       . ,superseded-proc)
       (dependent        . ,dependent-proc)
       (all              . ,all-proc))
      (output
       (id               . ,(apply-to-rest ids->output-patterns))
       (name             . ,(apply-to-rest specifications->output-patterns))
       (installed        . ,manifest-output-proc)
       (unknown          . ,(apply-to-first unknown-output-patterns))
       (regexp           . ,regexp-proc)
       (license          . ,license-proc)
       (location         . ,location-proc)
       (from-file        . ,file-proc)
       (from-os-file     . ,os-file-proc)
       (hidden           . ,hidden-proc)
       (superseded       . ,superseded-proc)
       (dependent        . ,dependent-proc)
       (all              . ,all-proc)))))

(define (patterns-maker entry-type search-type)
  (or (and=> (assq-ref %patterns-makers entry-type)
             (cut assq-ref <> search-type))
      (search-type-error entry-type search-type)))

(define (package/output-sexps profile entry-type
                              search-type search-values params)
  "Return information about packages or package outputs.

SEARCH-TYPE and SEARCH-VALUES define how to get the information.
SEARCH-TYPE should be one of the following symbols: 'id', 'name',
'regexp', 'all', 'installed', 'unknown', 'superseded',
'dependent', 'license', 'location', 'from-file','from-os-file'.

PARAMS is a list of parameters for receiving.  If it is an empty list,
get information with all available parameters, which are: 'id', 'name',
'version', 'outputs', 'license', 'synopsis', 'description', 'home-url',
'inputs', 'native-inputs', 'propagated-inputs', 'location',
'installed'."
  (let* ((manifest (profile-manifest profile))
         (patterns (if (and (eq? entry-type 'output)
                            (eq? search-type 'profile-diff))
                       (match search-values
                         ((p1 p2)
                          (map specification->output-pattern
                               (profile-difference p1 p2)))
                         (_ '()))
                       (apply (patterns-maker entry-type search-type)
                              manifest search-values)))
         (->sexps ((pattern-transformer entry-type) manifest params)))
    (to-emacs-side (append-map ->sexps patterns))))

(define (package-names)
  "Return a list of names of available packages."
  (fold-packages (lambda (pkg res)
                   (cons (package-name pkg) res))
                 '()))

(define (package-names*)
  "Return to emacs side a list of names of available packages."
  (to-emacs-side (package-names)))

(define (number-of-packages)
  "Return the number of available packages."
  (fold-packages (lambda (_ sum) (1+ sum))
                 0))


;;; Package locations

(define (package-location-string id-or-name)
  "Return location string of a package with ID-OR-NAME."
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

;;; packages.scm ends here
