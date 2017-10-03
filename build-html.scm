#!/usr/bin/env guile
!#
;;; build-html.scm --- Guile script to build html pages

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

(define-syntax-rule (push! elt lst)
  (set! lst (cons elt lst)))

(let* ((top-dir (dirname (current-filename)))
       (modules (string-append top-dir "/scheme")))
  (push! modules %load-path)
  ((module-ref (resolve-interface '(site))
               'build-pages)
   (string-append top-dir "/html-raw")))

;;; build-html.scm ends here
