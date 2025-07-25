;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
;;; Copyright © 2023, 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2023, 2024 David Elsing <david.elsing@posteo.net>
;;; Copyright @ 2022, Kitzman <kitzman@disroot.org>
;;; Copyright @ 2025 Dariqq <dariqq@posteo.net>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages))

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
