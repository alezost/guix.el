;;; site.scm --- The code to build html pages

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

(define-module (site)
  #:use-module (ice-9 match)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (site main)
  #:use-module (site doc)
  #:use-module (site screenshots)
  #:use-module (site utils)
  #:export (build-pages))

(define (build-page name directory page-maker)
  "Make NAME html page by calling PAGE-MAKER and write it to DIRECTORY."
  (let ((page (make-page name
                         `((doctype "html") ,(page-maker))
                         sxml->html)))
    (format #t "writing page '~a' to '~a'~%"
            (page-file-name page)
            directory)
    (write-page page directory)))

(define (build-pages directory)
  "Generate all html pages and put them into DIRECTORY."
  (define pages
    `(("index.html" ,main-page)
      ("doc.html" ,(lambda ()
                     (doc-page (make-url directory "manual"))))
      ("screenshots.html" ,screenshots-page)))

  (for-each (match-lambda
              ((name page-maker)
               (build-page name directory page-maker)))
            pages))

;;; site.scm ends here
