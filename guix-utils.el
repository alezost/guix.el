;;; guix-utils.el --- General utility functions

;; Copyright © 2014 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides auxiliary general functions for guix.el package.

;;; Code:

(defvar guix-true-string "Yes")
(defvar guix-false-string "–")
(defvar guix-list-separator ", ")

(defun guix-get-string (val &optional face)
  "Convert VAL into a string and return it.

VAL can be an expression of any type.
If VAL is t/nil, it is replaced with
`guix-true-string'/`guix-false-string'.
If VAL is list, its elements are concatenated using
`guix-list-separator'.

If FACE is non-nil, propertize returned string with this FACE."
  (let ((str (cond
              ((stringp val) val)
              ((null val) guix-false-string)
              ((eq t val) guix-true-string)
              ((numberp val) (number-to-string val))
              ((listp val) (mapconcat #'guix-get-string
                                      val guix-list-separator))
              (t (prin1-to-string val)))))
    (if face
        (propertize str 'face face)
      str)))

(defun guix-get-one-line (str)
  "Return one-line string from a multi-line STR."
  (replace-regexp-in-string "\n" " " str))

(defun guix-mapinsert (function sequence separator)
  "Like `mapconcat' but for inserting text.
Apply FUNCTION to each element of SEQUENCE, and insert SEPARATOR
at point between each FUNCTION call."
  (funcall function (car sequence))
  (mapc (lambda (obj)
          (insert separator)
          (funcall function obj))
        (cdr sequence)))

(provide 'guix-utils)

;;; guix-utils.el ends here
