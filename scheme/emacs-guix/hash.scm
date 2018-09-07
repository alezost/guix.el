;;; hash.scm --- Calculate file hash (for Guix packages)

;; Copyright © 2017–2018 Alex Kost <alezost@gmail.com>

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

(define-module (emacs-guix hash)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (gcrypt hash)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:export (file-hash))

;; XXX The code of the procedures below ('hash-format->procedure' and
;; 'vcs-file?') is taken from (guix scripts hash) module: unfortunately,
;; this code is not separated into stand-alone procedures, so it can't
;; be used otherwise.

(define (hash-format->procedure hash-format)
  "Return procedure for HASH-FORMAT to convert bytevector to a string."
  (match hash-format
    ("nix-base32"
     bytevector->nix-base32-string)
    ("base32"
     bytevector->base32-string)
    ((or "base16" "hex" "hexadecimal")
     bytevector->base16-string)
    (_
     (error (format #f "Unsupported hash format: ~a~%" hash-format)))))

(define (vcs-file? file stat)
  "Return #t if file is a version-controlled file."
  (case (stat:type stat)
    ((directory)
     (member (basename file) '(".bzr" ".git" ".hg" ".svn" "CVS")))
    ((regular)
     ;; Git sub-modules have a '.git' file that is a regular text file.
     (string=? (basename file) ".git"))
    (else
     #f)))

(define* (file-hash file-name #:key recursive? (format "nix-base32"))
  "Return a string with the hash of FILE-NAME.
If RECURSIVE? is #t, calculate the hash recursively excluding
version-controlled files."
  ((hash-format->procedure format)
   (if recursive?
       (let-values (((port get-hash) (open-sha256-port)))
         (write-file file-name port #:select? (negate vcs-file?))
         (force-output port)
         (get-hash))
       (call-with-input-file file-name port-sha256))))

;;; hash.scm ends here
