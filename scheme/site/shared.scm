;;; shared.scm --- Common code used to generate various pages

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

(define-module (site shared)
  #:use-module (site utils)
  #:export (guix-href
            emacs-guix-source-href
            manual-node-href
            emacs-command-tag
            kbd-tag
            head-tag
	    page-header))

(define* (guix-href #:key (title "GNU Guix"))
  "Return href tag for the Guix home page."
  `(a (@ (href ,%guix-url)) ,title))

(define* (emacs-guix-source-href #:key title url-end)
  "Return href tag for the Emacs-Guix source code URL.
URL-END is the part that prepended to the URL."
  (let ((url (if url-end
                 (make-url %emacs-guix-source-url url-end)
                 %emacs-guix-source-url)))
    `(a (@ (href ,url)) ,(or title url))))

(define* (manual-node-href node #:key title)
  "Return href tag for the manual NODE."
  `(a (@ (href ,(manual-url "latest/html_node"
                            (string-append node ".html"))))
      ,(or title node)))

(define (emacs-command-tag command)
  `(code ,(string-append "M-x " command)))

(define (kbd-tag key-binding)
  `(kbd ,key-binding))

(define* (head-tag title #:key (css (css-url "default.css")))
  `(head (meta (@ (http-equiv "Content-Type")
                  (content "text/html; charset=utf-8")))
	 (meta (@ (name "author")
		  (content ,%author-name)))
	 (meta (@ (name "description")
		  (content
		   "Emacs-Guix is the Emacs interface for GNU Guix.")))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href ,css)))
         (title ,(string-append title " — Emacs-Guix"))))

(define (page-header)
  `(div (@ (id "header"))
	(div (@ (id "header-name")) "Emacs-Guix")
	(ul (@ (id "header-links"))
	    (li (a (@ (href "index.html")) "About"))
	    (li (a (@ (href "doc.html")) "Documentation"))
	    (li (a (@ (href "screenshots.html")) "Screenshots")))))

;;; shared.scm ends here
