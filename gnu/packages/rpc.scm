;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022, 2024 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages rpc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

;; XXX: Starting from version 1.47.5, opencensus-proto is required.
;; The package is already deprecated upstream, so it's probably not
;; worth it spending time packaging it in Guix, just inject the source
;; instead, to also avoid us a recursive git fetch.
(define opencensus-proto-for-grpc-source
  (origin
    (method url-fetch)
    (uri "https://github.com/census-instrumentation/opencensus-proto/archive/v0.3.0.tar.gz")
    (sha256
     (base32 "1c3jfl1zgjhhqyqii1wils2k05akkvrw50xmf0q0rs2r885kzqdp"))))

(define-public grpc
  (package
    (name "grpc")
    (version "1.52.2")
    (outputs '("out" "static"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grpc/grpc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09165p6rh5xvcnnwnmy22lwdfchgjg39y02rwj6zg4rzfps8cb43"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test target
      #:configure-flags
      #~(list "-DgRPC_ZLIB_PROVIDER=package"
              "-DgRPC_ABSL_PROVIDER=package"
              "-DgRPC_CARES_PROVIDER=package"
              "-DgRPC_SSL_PROVIDER=package"
              "-DgRPC_PROTOBUF_PROVIDER=package"
              "-DgRPC_RE2_PROVIDER=package"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
              "-DCMAKE_INSTALL_LIBDIR=lib"
              (string-append "-DCMAKE_INSTALL_RPATH=" #$output "/lib")
              "-DCMAKE_VERBOSE_MAKEFILE=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-third-party
            (lambda _
              (mkdir-p "third_party/opencensus-proto/src")
              (invoke "tar" "xvf" #+opencensus-proto-for-grpc-source
                      "-C" "third_party/opencensus-proto/src")))
          (add-before 'configure 'configure-shared
            (lambda* (#:key configure-flags #:allow-other-keys)
              (mkdir "../build-shared")
              (with-directory-excursion "../build-shared"
                (apply invoke "cmake" "../source"
                       "-DBUILD_SHARED_LIBS=ON"
                       configure-flags)
                (apply invoke "make"
                       (list "-j" (number->string (parallel-job-count)))))))
          (add-after 'install 'install-shared-libraries
            (lambda _
              (with-directory-excursion "../build-shared"
                (invoke "make" "install"))))
          (add-before 'strip 'move-static-libs
            (lambda _
              (let ((static #$output:static))
                (mkdir-p (string-append static "/lib"))
                (with-directory-excursion (string-append #$output "/lib")
                  (for-each
                   (lambda (file)
                     (rename-file file (string-append static "/lib/" file)))
                   (find-files "." "\\.a$")))))))))
    (native-inputs
     (list pkg-config
           protobuf
           python-wrapper))
    (inputs
     (list c-ares/cmake
           openssl
           re2
           zlib))
    (propagated-inputs
     ;; Abseil libraries are in the 'Requires' field of the various gRPC
     ;; pkg-config files.
     (list abseil-cpp-cxxstd11))
    (home-page "https://grpc.io")
    (synopsis "High performance universal RPC framework")
    (description "gRPC is a modern high performance @dfn{Remote Procedure Call}
(RPC) framework that can run in any environment.  It can efficiently connect
services in and across data centers with pluggable support for load balancing,
tracing, health checking and authentication.  It is also applicable in last
mile of distributed computing to connect devices, mobile applications and
browsers to backend services.")
    (license license:asl2.0)))

(define-public python-grpc-stubs
  (package
    (name "python-grpc-stubs")
    (version "1.24.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "grpc-stubs" version))
              (sha256
               (base32
                "19dkm365g38lvxm799d29dnzg60g8in8251c18qkvsv4n92h8axh"))))
    (build-system python-build-system)
    (propagated-inputs (list python-grpcio python-typing-extensions))
    (home-page "https://github.com/shabbyrobe/grpc-stubs")
    (synopsis "Typing stubs for Python")
    (description "This is a PEP-561-compliant stub-only package which provides
type information of gRPC.")
    (license license:expat)))

(define-public python-grpcio
  (package
    (name "python-grpcio")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio" version))
       (sha256
        (base32 "1nsgm8q4yahzdab4m3irffdw9zklq4kb7f8hki1ayfgw54ysim55"))
       (modules '((guix build utils) (ice-9 ftw) (srfi srfi-26)))
       (snippet
        '(begin
           ;; Delete this generated file.
           (delete-file "src/python/grpcio/grpc/_cython/cygrpc.cpp")
           (with-directory-excursion "third_party"
             ;; Delete the bundled source code of libraries that are possible
             ;; to provide as inputs.
             (for-each delete-file-recursively
                       (scandir "." (negate (cut member <> '("." ".."
                                                             "address_sorting"
                                                             "upb"
                                                             "xxhash"))))))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ; There seems to be no tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'use-system-libraries
            (lambda _
              (substitute* "setup.py"
                (("EXTENSION_INCLUDE_DIRECTORIES = \\(" m)
                 (string-append m " ('" #$(this-package-input "grpc")
                                "/include/grpc/impl/codegen/',) + ")))
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_CARES" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_OPENSSL" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_ZLIB" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_RE2" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_ABSL" "1")
              (setenv "GRPC_PYTHON_BUILD_WITH_CYTHON" "1")
              ;; Fix the linker options to link with abseil-cpp, which is
              ;; looked under /usr/lib.
              (substitute* "setup.py"
                (("pathlib.Path\\('/usr').glob\\('lib\\*/libabsl_\\*.so')")
                 (format #f "pathlib.Path('~a').glob('lib*/libabsl_*.so')"
                         #$(this-package-input "abseil-cpp"))))))
          (add-before 'build 'configure-compiler
            (lambda _
              (substitute* '("setup.py" "src/python/grpcio/commands.py")
                (("'cc'") "'gcc'")))))))
    (inputs (list abseil-cpp-cxxstd11 c-ares grpc openssl re2 zlib))
    (native-inputs (list python-cython-0 python-setuptools))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework")
    (description
     "This package provides a Python library for communicating with the
HTTP/2-based RPC framework gRPC.")
    (license license:asl2.0)))

(define-public python-grpcio-tools
  (package
    (name "python-grpcio-tools")
    (version "1.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grpcio-tools" version))
       (modules '((guix build utils)))
       (snippet
        ;; This file is auto-generated.
        '(delete-file "grpc_tools/_protoc_compiler.cpp"))
       (sha256
        (base32 "0m5xwhz3l0n3b1bzjncynwflnc5iyv4xrjq046ppcck4rpj9fgn0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f       ;XXX: no tests in PyPI, try to bulid from Git
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure
            (lambda _
              (setenv "GRPC_PYTHON_BUILD_WITH_CYTHON" "1"))))))
    (native-inputs (list python-cython python-setuptools))
    (propagated-inputs (list python-grpcio python-protobuf-4))
    (home-page "https://grpc.io")
    (synopsis "Protobuf code generator for gRPC")
    (description
     "The gRPC tools for Python provide a special plugin for generating server
and client code from @file{.proto} service definitions.")
    (license license:asl2.0)))

(define-public apache-thrift
  (package
    (name "apache-thrift")
    (version "0.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wmnb3h0xq8qc5a9g9lliszh6qg254f5856h72viab46bizmdd4a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           flex
           bison))
    (inputs
     (list boost libressl))
    (outputs '("out" "lib" "include"))
    (home-page "https://thrift.apache.org/")
    (synopsis
     "Lightweight, language-independent software stack for point-to-point
RPC")
    (description
     "Thrift provides clean abstractions and implementations for data
transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates
code across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license license:asl2.0)))
