;;; utils.scm --- General utilities used by other modules

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

(define-module (site utils)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%author-name
            %guix-url
            %emacs-guix-source-url
            directory-files
            make-url
            css-url
            manual-url
            image-url))

(define %author-name "Alex Kost")
(define %guix-url "http://www.gnu.org/software/guix/")
(define %emacs-guix-source-url "https://github.com/alezost/guix.el")

(define (make-url first-part . rest-parts)
  "Return file name by concatenating FILE-PARTS with slashes."
  (fold (lambda (elt res)
          (string-append (string-trim-right res #\/)
                         "/"
                         (string-trim elt #\/)))
        first-part
        rest-parts))

(define (css-url name)
  (make-url "css" name))

(define (manual-url . url-parts)
  (apply make-url "manual" url-parts))

(define (image-url . url-parts)
  (apply make-url "images" url-parts))

(define (directory-files directory)
  "Return all file names from DIRECTORY."
  (scandir directory
           (negate (cut member <> '("." "..")))))

;;; utils.scm ends here
