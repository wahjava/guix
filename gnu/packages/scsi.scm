;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages scsi)
  #:use-module ((guix licenses)
                #:select (gpl2+ bsd-2 bsd-3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public sg3-utils
  (package
    (name "sg3-utils")
    (version "1.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://sg.danny.cz/sg/p/sg3_utils-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "04l58wvncqk6lfh4lashsl3ccsa1114hkd6vwi1h1cbmq2fw9c8v"))))
    (build-system gnu-build-system)
    (home-page "http://sg.danny.cz/sg/sg3_utils.html")
    (synopsis "SCSI device utilities")
    (description
     "sg3-utils is a collection of utilities for devices that use the Small
Computer System Interface (SCSI) command set.  It includes utilities to read
data from, write data to, control, modify, and query the state of SCSI
devices.  For example, this package provides command-line tools to copy data
based on @code{dd} syntax and semantics (called @code{sg_dd}, @code{sgp_dd}
and @code{sgm_dd}), check INQUIRY data and VPD pages (@code{sg_inq}), check
mode and log pages (@code{sginfo}, @code{sg_modes} and @code{sg_logs}), spin
up and down disks (@code{sg_start}), do self tests (@code{sg_senddiag}), parse
sense data (@code{sg_decode_sense}), and perform various other functions.  In
addition, this package includes a library, called libsgutils, which can be
used in C and C++ programs to interact with SCSI devices.")
    ;; See README: "All utilities and libraries have either a "2 clause" BSD
    ;;   license or are "GPL-2ed". [...] That BSD license was updated from the
    ;;   "3 clause" to the newer "2 clause" version on 20180119. To save space
    ;;   various source code files refer to a file called "BSD_LICENSE" [...]."
    ;; Some files (like sg_compare_and_write.c) retain their 3-clause headers!
    (license (list gpl2+ bsd-2 bsd-3))))
