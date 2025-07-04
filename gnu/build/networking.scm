;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2025 Romain Garbage <romain.garbage@inria.fr>
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

(define-module (gnu build networking)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (cidr->ip
            cidr->netmask
            ip+netmask->cidr
            ipv6-address?
            mac-address?))

(define (cidr->ip str)
  "Strip the netmask bit of @var{str}, a CIDR-notation IP/netmask address."
  (match (string-split str #\/)
    ((or (ip _) (ip))
     ip)))

(eval-when (expand load eval)
  (define* (cidr->netmask str #:optional (family AF_INET))
    "Given @var{str}, a string in CIDR notation (e.g., \"1.2.3.4/24\"), return
the netmask as a string like \"255.255.255.0\"."
    (match (string-split str #\/)
      ((ip (= string->number bits))
       (let ((mask (ash (- (expt 2 bits) 1)
                        (- (if (= family AF_INET6) 128 32)
                           bits))))
         (inet-ntop family mask)))
      (_ #f))))

(define* (ip+netmask->cidr ip netmask #:optional (family AF_INET))
  "Return the CIDR notation (a string) for @var{ip} and @var{netmask}, two
@var{family} address strings, where @var{family} is @code{AF_INET} or
@code{AF_INET6}."
  (let* ((netmask (inet-pton family netmask))
         (bits    (logcount netmask)))
    (string-append ip "/" (number->string bits))))

(define (ipv6-address? str)
  "Return true if STR denotes an IPv6 address."
  (false-if-exception (->bool (inet-pton AF_INET6 str))))

(define (mac-address? str)
  "Return true if STR is a valid MAC address."
  (let ((pattern (make-regexp "^([0-9A-Fa-f]{2}:?){6}$")))
    (false-if-exception (vector? (regexp-exec pattern str)))))
