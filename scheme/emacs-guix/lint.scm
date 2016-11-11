;;; lint.scm --- Code related to linting Guix packages

;; Copyright © 2015 Alex Kost <alezost@gmail.com>

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

(define-module (emacs-guix lint)
  #:use-module (guix scripts lint)
  #:export (lint-checker-names))

(define (lint-checker-names)
  "Return a list of names of available lint checkers."
  (map (lambda (checker)
         (symbol->string (lint-checker-name checker)))
       %checkers))

;;; lint.scm ends here
