;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Florian Pelz <pelzflorian@pelzflorian.de>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;; Minify ./po/packages files when synced from
;; <https://codeberg.org/guix/translations/>,
;; keeping only actually translated messages.

(use-modules (guix build po)
             (ice-9 match)
             (ice-9 textual-ports))

(define (escape str)
  "Replace C-style escape sequences.  Also Gettext needs lines to be enclosed
in quotation marks."
  (let ((in (open-input-string str)))
    (let loop ((line (get-line in)))
      (let* ((escaped-line-as-list
              (string-fold-right
               (lambda (char result)
                 (cons (case char
                         ((#\") "\\\"")
                         ((#\\) "\\\\")
                         ((#\return) "\\r")
                         ((#\tab) "\\t")
                         (else (string char)))
                       result))
               '()
               line))
             (escaped-line (apply string-append escaped-line-as-list)))
        (display "\"")
        (display escaped-line))
      (let ((next-line (get-line in)))
        (if (eof-object? next-line)
            (display "\"")
            (begin
              (display "\\n\"")
              (newline)
              (loop next-line)))))))

(match (command-line)
  ((program pofile)
   (let ((input (open-input-file pofile)))
     ;; Just copy until an empty line.
     (letrec ((copy
               (lambda ()
                 (let ((next-line (get-line input)))
                   (display next-line)
                   (newline)
                   (when (> (string-length next-line) 0)
                     (copy))))))
       (copy))
     ;; Then print only translated messages.
     (for-each
      (lambda (msg)
        (match msg
          ((msgid . msgstr)
           (display "msgid \"\"")
           (newline)
           (escape msgid)
           (newline)
           (display "msgstr \"\"")
           (newline)
           (escape msgstr)
           (newline)
           (newline))))
      (read-po-file input)))))
