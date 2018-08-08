;;; commands.scm --- Executing Guix commands

;; Copyright © 2015-2016, 2018 Alex Kost <alezost@gmail.com>

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

;; This module provides the code to run 'guix' commands, return their
;; outputs, etc.

;;; Code:

(define-module (emacs-guix commands)
  #:use-module (ice-9 popen)
  #:use-module (guix ui)
  #:export (guix-command
            guix-command-output
            help-string
            guix-output-to-file
            pipe-guix-output))

(define (output+error thunk)
  "Call THUNK and return 2 values: output and error output as strings."
  (let ((output-port (open-output-string))
        (error-port  (open-output-string)))
    (with-output-to-port output-port
      (lambda () (with-error-to-port error-port thunk)))
    (let ((strings (list (get-output-string output-port)
                         (get-output-string error-port))))
      (close-output-port output-port)
      (close-output-port error-port)
      (apply values strings))))

(define (guix-command . args)
  "Run 'guix ARGS ...' command."
  (catch 'quit
    (lambda () (apply run-guix args))
    (const #t)))

(define (guix-command-output . args)
  "Return 2 strings with 'guix ARGS ...' output and error output."
  (output+error
   (lambda ()
     (parameterize ((guix-warning-port (current-error-port)))
       (apply guix-command args)))))

(define (help-string . commands)
  "Return string with 'guix COMMANDS ... --help' output."
  (apply guix-command-output `(,@commands "--help")))

(define (guix-output-to-file guix-args file-name)
  "Run 'guix GUIX-ARGS ...' command and redirect its output to FILE-NAME."
  (with-output-to-file file-name
    (lambda () (apply guix-command guix-args))))

(define (pipe-guix-output guix-args command-args)
  "Run 'guix GUIX-ARGS ...' command and pipe its output to a shell command
defined by COMMAND-ARGS.
Return #t if the shell command was executed successfully."
  (let ((pipe (apply open-pipe* OPEN_WRITE command-args)))
    (with-output-to-port pipe
      (lambda () (apply guix-command guix-args)))
    (zero? (status:exit-val (close-pipe pipe)))))

;;; commands.scm ends here
