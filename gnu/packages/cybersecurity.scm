;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 c4droid <c4droid@foxmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages cybersecurity)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system python)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages bioinformatics)      ;python-intervaltree
  #:use-module (gnu packages emulators))

(define-public blacksmith
  (package
    (name "blacksmith")
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/comsec-group/blacksmith")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kyp71wndf527dgza5iks5m5vj543mvxp5w7cjd8x0pilmd1xrls"))
              (modules '((guix build utils)))
              (snippet `(begin
                          (delete-file-recursively "external")
                          (substitute* "CMakeLists.txt"
                            (("add_subdirectory\\(external\\)") "")
                            (("[ \t]*FetchContent_MakeAvailable\\(asmjit\\)")
                             (string-append
                              "find_package(asmjit)\n"
                              "find_package(nlohmann_json)")))))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test-suite
       #:imported-modules
       ((guix build copy-build-system)
        ,@%cmake-build-system-modules)
       #:modules
       (((guix build copy-build-system) #:prefix copy:)
        (guix build cmake-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; Use default C++ standard instead.
               (("cxx_std_17") "")
               ;; This project tries to link argagg library, which doesn't
               ;; exist, as argagg project is a single header file.
               (("argagg") ""))))
         (replace 'install
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("." "bin" #:include ("blacksmith"))
                      ("." "lib" #:include-regexp ("\\.a$")))
                    args))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list argagg asmjit nlohmann-json))
    (home-page "https://comsec.ethz.ch/research/dram/blacksmith")
    (synopsis "Rowhammer fuzzer with non-uniform and frequency-based patterns")
    (description
     "Blacksmith is an implementation of Rowhammer fuzzer that crafts novel
non-uniform Rowhammer access patterns based on the concepts of frequency,
phase, and amplitude.  It is able to bypass recent @acronym{TRR, Target Row
Refresh}in-DRAM mitigations effectively and as such can trigger bit flips.")
    (license license:expat)))

(define-public ropgadget
  (package
    (name "ropgadget")
    (version "6.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ROPGadget" version))
       (sha256
        (base32 "08ms7x4af07970ij9899l75sghnxsa7xyx73gkn6gv0l05p1hqfw"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-capstone))
    (home-page "https://shell-storm.org/project/ROPgadget/")
    (synopsis "Semiautomatic return oriented programming")
    (description
     "This tool lets you search for @acronym{ROP, Return Oriented Programming}
gadgets in binaries.  Some facilities are included for automatically generating
chains of gadgets to execute system calls.")
    (license license:bsd-3)))

(define-public pwntools
  (package
    (name "pwntools")
    (version "4.15.0b1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pwntools" version))
       (sha256
        (base32
         "091fsk9rvbjkcsp8mmww0ka26dvznmj4pbqwaiygcw90g3v94zgd"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))                 ;XXX: needs a specific version of unicorn
    (propagated-inputs
     (list capstone
           python-colored-traceback
           python-dateutil
           python-intervaltree
           python-mako
           python-packaging
           python-paramiko
           python-pathlib2
           python-psutil
           python-pyelftools
           python-pygments
           python-pyserial
           python-pysocks
           python-requests
           ropgadget
           python-rpyc
           python-six
           python-sortedcontainers
           python-unix-ar
           python-zstandard
           unicorn))
    (native-inputs
     (list python-setuptools python-toml python-wheel))
    (home-page "https://github.com/Gallopsled/pwntools")
    (synopsis
     "Capture-the-flag (CTF) framework and exploit development library")
    (description
     "Pwntools is a capture-the-flag (CTF) framework and exploit development library.
Written in Python, it is designed for rapid prototyping and development, and
intended to make exploit writing as simple as possible.")
    (license license:expat)))

(define-public gef-src
  (package
    (name "gef-src")
    (version "2025.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hugsy/gef")
             (commit "ed10244b0fe4665e8ee03fa326b1b1c711b39563")))
       (sha256
        (base32 "1b6c1rqhk6950phyal7pcxfy7qxm0p1c9rx70plxh48nbhgp7kr4"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("gef.py" "share/"))))
    (inputs (list python-3.11 gcc-toolchain file gdb))
    (synopsis "gdb extension for exploit development and vulnerability
              research.")
    (description
     "@acronym{GEF, Gdb Extended Functions} is a gdb extension for
                 exploit development and security research.it includes
                 enhancements for heap analysis and vulnerability detection, as
                 well as nice ways to view memory.")
    (home-page "https://hugsy.github.io/gef/")
    (license license:expat)))

(define (make-gef gdb-exec)
  (package
    (name (string-append "gef-" (package-name gdb-exec)))
    (version (package-version gdb-exec))
    (source
     (program-file "gef"
        (with-imported-modules '((guix build utils) (srfi srfi-1))
        #~(begin
            (use-modules (guix build utils) (srfi srfi-1))
                (system (string-append
                      "gdb -q -x "
                      #$gef-src
                      "/share/gef.py "
                      (fold-right string-append ""
                                  (map (lambda x (string-append " " (car x)))
                                       (cdr (command-line))))))))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("gef" "bin/"))))
    (propagated-inputs (list python-3.11 gcc-toolchain file gdb-exec gef-src))
    (synopsis "script to run gdb with @acronym{GEF,Gdb Extended Functions}
    extension")
    (description "This package provides an executable, @command{gef} ,which
    executes gdb with the gef extension.")
    (home-page "https://hugsy.github.io/gef/")
    (license license:gpl3+)))

(define-public gef-gdb-15
  (make-gef gdb-15))

(define-public gef-gdb-14
  (make-gef gdb-16))

(define-public gef-gdb-16
  (make-gef gdb-16))

(define-public gef-gdb-multiarch
  (make-gef gdb-multiarch))
