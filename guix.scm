;;; guix.scm --- Guix package for Emacs-Guix

;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>

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

;;; Commentary:

;; This file contains Guix package for development version of
;; Emacs-Guix.  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm

;; The main purpose of this file though is to make a development
;; environment for building Emacs-Guix:
;;
;;   guix environment --pure --load=guix.scm
;;   ./autogen.sh
;;   ./configure
;;   make

;;; Code:

(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (guix build utils)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (gnu packages autotools)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages pkg-config)
 (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(define emacs-guix-devel
  (let ((commit (current-commit)))
    (package
      (inherit emacs-guix)
      (version (string-append (package-version emacs-guix)
                              "-" (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (arguments
       (append (package-arguments emacs-guix)
               '(#:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'autogen
                     (lambda _ (zero? (system* "sh" "autogen.sh"))))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ;; 'emacs-minimal' does not find Emacs packages (this is for
         ;; "guix environment").
         ("emacs" ,emacs-no-x)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo))))))

emacs-guix-devel

;;; guix.scm ends here
