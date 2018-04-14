;;; emacs.scm --- Variables and procedures to interact with the Emacs side

;; Copyright © 2018 Alex Kost <alezost@gmail.com>

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

(define-module (emacs-guix emacs)
  #:export (%max-returned-list-size
            %temporary-directory
            to-emacs-side))

;; Maximal length of a list that is passed to the Emacs side directly
;; through Geiser.  If a returning list has a bigger length, it will be
;; written to `(temporary-output-file)' and this file name will be
;; returned instead.  If #f, then a list of any size will be returned.
(define %max-returned-list-size #f)

(define %temporary-directory "/tmp")

(define (temporary-output-file)
  (string-append %temporary-directory "/output.scm" ))

(define (to-emacs-side object)
  "Return OBJECT if it is not too big.
Otherwise, write it to a file and return the cons:

  (in-file . FILE-NAME)"
  (if (and (integer? %max-returned-list-size)
           (list? object)
           (or (< %max-returned-list-size 1)
               (< %max-returned-list-size (length object))))
      (let ((file-name (temporary-output-file)))
        (with-output-to-file file-name
          (lambda () (write object)))
        (cons 'in-file file-name))
      object))

;;; emacs.scm ends here
