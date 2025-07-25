;;; GNU Guix --- Functional package management for GNU
;;; Copyright @ 2022, Kitzman <kitzman@disroot.org>
;;; Copyright @ 2025 Dariqq <dariqq@posteo.net>
;;; Copyright © 2012, 2013, 2014, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023, 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2023, 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages lex-yacc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public byacc
  (package
    (name "byacc")
    (version "20240109")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://invisible-mirror.net/archives/byacc/byacc-"
                   version ".tgz"))
             (sha256
              (base32
               "0il4w1vwbglayakywyghiqhcjpg1yvv5ww2p8ylz32bi05wpg2gj"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/byacc/byacc.html")
    (synopsis "Berkeley Yacc LALR parser generator")
    (description
     "Berkeley Yacc is an LALR(1) parser generator.  Yacc reads the grammar
specification from a file and generates an LALR(1) parser for it.  The parsers
consist of a set of LALR(1) parsing tables and a driver routine written in the
C programming language.")
    (license license:public-domain)))

(define-public flex
  (package
    (name "flex")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/westes/flex"
                    "/releases/download/v" version "/"
                    "flex-" version ".tar.gz"))
              (sha256
               (base32
                "15g9bv236nzi665p9ggqjlfn4dwck5835vf0bbw2cz7h5c1swyp8"))))
    (build-system gnu-build-system)
    (inputs
     (let ((bison-for-tests
            (package
              (inherit bison)
              (arguments
               ;; Disable tests, since they require flex.
               (substitute-keyword-arguments (package-arguments bison)
                 ((#:tests? _ #f) #f)))
              (inputs (alist-delete "flex" (package-inputs bison))))))
       `(("bison" ,bison-for-tests))))
    (arguments
     (if (or (target-hurd64?) (%current-target-system))
         (list #:configure-flags
               #~'("ac_cv_func_malloc_0_nonnull=yes"
                   "ac_cv_func_realloc_0_nonnull=yes"
                   #$(string-append
                      "CFLAGS=-g -O2"
                      " -Wno-error=implicit-function-declaration"
                      " -Wno-error=int-conversion")))
         '()))
    ;; m4 is not present in PATH when cross-building
    (native-inputs
     (list help2man m4))
    (propagated-inputs (list m4))
    (home-page "https://github.com/westes/flex")
    (synopsis "Fast lexical analyser generator")
    (description
     "Flex is a tool for generating scanners.  A scanner, sometimes
called a tokenizer, is a program which recognizes lexical patterns in
text.  The flex program reads user-specified input files, or its standard
input if no file names are given, for a description of a scanner to
generate.  The description is in the form of pairs of regular expressions
and C code, called rules.  Flex generates a C source file named,
\"lex.yy.c\", which defines the function yylex().  The file \"lex.yy.c\"
can be compiled and linked to produce an executable.  When the executable
is run, it analyzes its input for occurrences of text matching the
regular expressions for each rule.  Whenever it finds a match, it
executes the corresponding C code.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))
