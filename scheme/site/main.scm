;;; main.scm --- Main page

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

(define-module (site main)
  #:use-module (site shared)
  #:use-module (site utils)
  #:export (main-page))

(define (main-page)
  "Return the main page."
  `(html
    (@ (lang "en"))
    ,(head-tag "About")
    (body
     ,(page-header)
     (h2 "About")
     (p (b "Emacs-Guix") " (aka " (b "guix.el") ")"
        " is the Emacs interface for "
        ,(guix-href) " package manager.")
     (p "Emacs-Guix provides "
        ,(manual-node-href "Introduction"
                           #:title "various features")
        ", in particular it allows
you to manage your Guix profile(s): to install, upgrade and remove
packages, to switch and remove profile generations, to display all
available info about packages and to do many other things.")
     (p "You can install it from "
        (a (@ (href "https://melpa.org/#/guix")) "MELPA")
        " or using Guix:")
     (pre (@ (class "example")) "guix package -i emacs-guix")
     (p "Source code of Emacs-Guix: " ,(emacs-guix-source-href))
     (p "Source code of this site: "
        ,(emacs-guix-source-href #:url-end "tree/gh-pages")))))

;;; main.scm ends here
