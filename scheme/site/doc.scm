;;; doc.scm --- "Documentation" page

;; Copyright © 2017 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (site doc)
  #:use-module (ice-9 match)
  #:use-module (site shared)
  #:use-module (site utils)
  #:export (doc-page))

(define %manual-names
  '(("latest" "Latest (from git)")))

(define (directory-name->manual-name directory)
  "Return manual name by the DIRECTORY name."
  (match (assoc directory %manual-names)
    ((_dir-name manual-name) manual-name)
    (_ directory)))

(define (manual-table dir-names)
  "Return table for the manual placed in DIR-NAMES."
  (define* (table-row dir-name #:optional manual-name)
    `(tr (td ,(or manual-name dir-name))
         (td (a (@ (href ,(manual-url dir-name "emacs-guix.html")))
                "html"))
         (td (a (@ (href ,(manual-url dir-name "html_node" "index.html")))
                "html"))))

  `(table (thead (tr (th "Version")
                     (th "Entirely on one web page")
                     (th "One web page per node")))
          (tbody ,@(map (lambda (dir-name)
                          (table-row dir-name
                                     (directory-name->manual-name dir-name)))
                        dir-names))))

(define (doc-page directory)
  "Return the documentation page for the manuals from DIRECTORY."
  (define dir-names
    (sort-list (directory-files directory)
               string>))

  `(html
    (@ (lang "en"))
    ,(head-tag "Documentation")
    (body
     ,(page-header)
     (h2 "Documentation")
     ,(manual-table dir-names)
     (p "The above HTML pages were generated from the Texinfo
manual that comes with Emacs-Guix.  Since you probably use Emacs, you
can always read this manual by following Emacs-Guix entry in "
        ,(kbd-tag "C-h i") " (" ,(emacs-command-tag "info") ")"
        " or simply with " ,(emacs-command-tag "guix-info") "."))))

;;; doc.scm ends here
