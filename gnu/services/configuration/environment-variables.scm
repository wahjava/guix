;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services configuration environment-variables)
  #:use-module (gnu services configuration)
  #:use-module (gnu services configuration utils)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:export (configuration->environment-variables))

(define* (field-name->environment-variable field-name #:key (prefix #f))
  "Serializes FIELD-NAME, a field name from @code{(gnu services configuration)},
to a snake case upper case string representation of the field name.  Trailing
@code{?} in the name are dropped and @code{-} get replaced by @code{_}.  When
PREFIX is a string, it is prepended to the result.

For example the procedure would convert @code{'A-Field?} to @code{\"A_FIELD\"}."
  (let ((variable (string-upcase
                   (uglify-snake-case field-name))))
    (if (string? prefix)
        (string-append prefix variable)
        variable)))

(define* (serialize-string-environment-variable field-name value
                                                #:key (prefix #f))
  (cons (field-name->environment-variable field-name #:prefix prefix)
        value))

(define* (serialize-boolean-environment-variable field-name value
                                                 #:key (prefix #f)
                                                 (true-value "true")
                                                 (false-value "false"))
  (serialize-string-environment-variable
   field-name
   (if value true-value false-value)
   #:prefix prefix))

(define* (serialize-number-environment-variable field-name value
                                                #:key (prefix #f))
  (cons (field-name->environment-variable field-name #:prefix prefix)
        (number->string value)))

(define* (configuration->environment-variables config fields
                                               #:key (excluded '())
                                               (prefix #f)
                                               (true-value "true")
                                               (false-value "false"))
  "Serializes CONFIG, a configuration from @code{(gnu services configuration)},
and its FIELDS to a list of pairs.  Each  pair represents an environment
variable.  The first element of each pair is the variable name, the second is
the value.  When PREFIX is a string it is prepended to the variable name.  If
any of FIELDS' names are a member of EXCLUDED they won't be serialized.
TRUE-VALUE and FALSE-VALUE will be used as a representation for respectfully
@code{#t} and @code{#f}."
  ;; Filter out unset maybe-types.
  (filter (compose not null?)
          (map (lambda (f)
                 (let ((field-name (configuration-field-name f))
                       (type (configuration-field-type f))
                       (value ((configuration-field-getter f) config)))
                   (if (not (member field-name excluded))
                       (match type
                         ('string
                          (serialize-string-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-string
                          (if (maybe-value-set? value)
                              (serialize-string-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('number
                          (serialize-number-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-number
                          (if (maybe-value-set? value)
                              (serialize-number-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('positive
                          (serialize-number-environment-variable
                           field-name value #:prefix prefix))
                         ('maybe-positive
                          (if (maybe-value-set? value)
                              (serialize-number-environment-variable
                               field-name value #:prefix prefix)
                              '()))
                         ('boolean
                          (serialize-boolean-environment-variable
                           field-name value #:prefix prefix
                           #:true-value true-value #:false-value false-value))
                         ('maybe-boolean
                          (if (maybe-value-set? value)
                              (serialize-boolean-environment-variable
                               field-name value #:prefix prefix
                               #:true-value true-value
                               #:false-value false-value)
                              '()))
                         (_
                          (raise
                           (formatted-message
                            (G_ "Unknown environment-variable field type: ~a")
                            type))))
                       '())))
               fields)))
