;;; graph.scm --- Code related to Guix graphs

;; Copyright © 2015–2017 Alex Kost <alezost@gmail.com>

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

;;; Code:

(define-module (emacs-guix graph)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (guix grafts)
  #:use-module (guix graph)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix scripts graph)
  #:autoload   (emacs-guix packages) (package-by-id-or-name)
  #:export (graph-backend-names
            graph-node-type-names
            make-package-graph))

(define (graph-backend-names)
  "Return a list of names of available graph backends."
  (map graph-backend-name %graph-backends))

(define (graph-node-type-names)
  "Return a list of names of available graph node types."
  (map node-type-name %node-types))

;; Original 'lookup-node-type' and 'lookup-backend' procedures from
;; (guix scripts graph) module are not suitable because they call
;; 'leave' when lookup fails.

(define (lookup-node-type name)
  "Return the node type called NAME."
  (find (lambda (type)
          (string=? (node-type-name type) name))
        %node-types))

(define (lookup-backend name)
  "Return the graph backend called NAME."
  (find (lambda (backend)
          (string=? (graph-backend-name backend) name))
        %graph-backends))

(define* (package-graph package port #:key node-type backend)
  "Write PACKAGE graph representation to PORT."
  ;; TODO This is blindly taken from 'guix-graph' procedure of (guix
  ;; scripts graph) module.  Since we have a single PACKAGE, there
  ;; should definitely be a way to simplify this (to avoid 'mapm' at
  ;; least), but I have no idea how to deal with monads.
  (with-store store
    (with-fluids ((%file-port-name-canonicalization 'absolute))
      (run-with-store store
        (mlet %store-monad ((_     (set-grafting #f))
                            (nodes (mapm %store-monad
                                         (node-type-convert node-type)
                                         (list package))))
          (export-graph (concatenate nodes)
                        port
                        #:node-type node-type
                        #:backend backend))))))

(define* (make-package-graph package-id-or-name command-or-file
                             #:key node-type-name backend-name)
  "Make graph for PACKAGE-ID-OR-NAME.

If COMMAND-OR-FILE is a list, pipe the graph representation to the shell
command defined by this list of command arguments (usually a 'dot'
call).

If COMMAND-OR-FILE is a file name, write graph output to this file.

Return #t if the graph was created successfully; return #f otherwise."
  (and-let* ((package   (package-by-id-or-name package-id-or-name))
             (node-type (lookup-node-type node-type-name))
             (backend   (lookup-backend backend-name)))
    (let ((graph-to-port (cut package-graph package <>
                              #:node-type node-type
                              #:backend backend)))
      (if (list? command-or-file)
          (let ((pipe (apply open-pipe* OPEN_WRITE command-or-file)))
            (graph-to-port pipe)
            (zero? (status:exit-val (close-pipe pipe))))
          (with-output-to-file command-or-file
            (lambda ()
              (graph-to-port (current-output-port))
              #t))))))

;;; graph.scm ends here
