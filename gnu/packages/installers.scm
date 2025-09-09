;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
;;; Copyright © 2025 fanquake <fanquake@gmail.com>
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

(define-module (gnu packages installers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages bash)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system scons)
  #:use-module (guix utils))

(define (make-nsis machine target-arch nsis-target-type)
  (let* ((triplet (string-append machine "-" "w64-mingw32"))
         (xbinutils (cross-binutils triplet))
         (xlibc (cross-libc triplet))
         (xgcc (cross-gcc triplet #:libc xlibc)))
    (package
      (name (string-append "nsis-" machine))
      (version "3.11")
      (source (origin
                (method url-fetch)
                (uri (string-append "http://prdownloads.sourceforge.net/nsis/nsis-"
                                    version "-src.tar.bz2"))
                (sha256
                 (base32
                  "0jzz07acshml9fq60v48sgzxp74p1fl2n0yw25ycdgbfcxi21rqr"))
                (patches (search-patches "nsis-env-passthru.patch"))))
      (build-system scons-build-system)
      (native-inputs `(("xgcc" ,xgcc)
                       ("xbinutils" ,xbinutils)
                       ("mingw-w64" ,xlibc)
                       ("bash" ,bash-minimal)))
      (inputs (list zlib))
      (arguments
       (list
        #:scons scons-3
        #:modules '((srfi srfi-1)
                    (srfi srfi-26)
                    (guix build utils)
                    (guix build scons-build-system))
        #:tests? #f
        #:scons-flags #~(cons*
                         "UNICODE=yes"
                         "SKIPUTILS=MakeLangId,Makensisw,NSIS Menu,SubStart,zip2exe"
                         "SKIPDOC=COPYING"
                         "STRIP_CP=no"
                         (string-append "PREFIX=" #$output)
                         (string-append "TARGET_ARCH=" #$target-arch)
                         (string-append "XGCC_W32_PREFIX=" #$triplet "-")
                         (string-append "PREFIX_PLUGINAPI_INC=" #+(this-package-native-input "mingw-w64") "/include/")
                         (string-append "PREFIX_PLUGINAPI_LIB=" #+(this-package-native-input "mingw-w64") "/lib/")
                         (if #$(%current-target-system)
                             (list
                              (string-append "CC=" #$(cc-for-target) "-wrapper")
                              (string-append "CXX=" #$(cxx-for-target) "-wrapper"))
                             '()))
        #:build-targets ''("makensis"
                           "stubs"
                           "plugins"
                           "utils")
        #:install-targets ''("install-stubs"
                             "install-plugins"
                             "install-data"
                             "install-utils"
                             "install-compiler"
                             "install-conf")
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'fix-env
              (lambda* (#:key target #:allow-other-keys)
                (define* (filter-delimited-string delimited-string predicate #:optional (delimiter #\:))
                  ;; Given a DELIMITED-STRING delimited by DELIMITER,
                  ;; only keep items that satisfy PREDICATE
                  (string-join
                   (filter predicate (string-split delimited-string delimiter))
                   (string delimiter)))
                (define (mingw-path? path)
                  (string-prefix? (assoc-ref %build-inputs "mingw-w64") path))
                (define (xgcc-path? path)
                  (string-prefix? (assoc-ref %build-inputs "xgcc") path))
                (define (cross-toolchain-path? path)
                  (or (xgcc-path? path) (mingw-path? path)))

                (for-each
                 (lambda (env-name)
                   (let ((env-val (getenv env-name)))
                     ;; Remove all mingw-w64 and xgcc paths from env vars meant
                     ;; for native toolchain
                     (setenv env-name
                             (filter-delimited-string env-val (negate cross-toolchain-path?)))
                     ;; Add the removed paths back into CROSS_-prefixed
                     ;; version of env vars
                     (setenv (string-append "CROSS_" env-name)
                             (filter-delimited-string env-val cross-toolchain-path?))))
                 '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH" "LIBRARY_PATH"))))
            (add-before 'fix-env 'fix-cross-compilation
              (lambda* (#:key target #:allow-other-keys)
                (when target
                  (mkdir-p "./cc-wrapper")
                  (setenv "PATH"
                          (string-append
                           (getcwd) "/cc-wrapper"
                           ":"
                           (getenv "PATH")))

                  ;; Make a wrapper for the CC and CXX, setting proper
                  ;; CROSS_* env vars
                  (call-with-output-file "./cc-wrapper/env"
                    (lambda (port)
                      (format port "#!~a/bin/sh~%" #$(this-package-native-input "bash"))
                      (for-each
                       (lambda (env-var)
                         (format port
                                 "export ~a=\"~a\"~%"
                                 env-var
                                 (getenv env-var))
                         (unsetenv env-var))
                         '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH" "CROSS_LIBRARY_PATH"))))

                  (for-each
                   (lambda (file)
                     (let ((target-file
                            (string-append "./cc-wrapper/" #$(%current-target-system) "-" file "-wrapper")))
                       (call-with-output-file target-file
                         (lambda (port)
                           (format port "#!~a/bin/sh~%" #$(this-package-native-input "bash"))
                           (format port
                                   "source ~a/cc-wrapper/env~%exec ~a-~a \"$@\""
                                   (getcwd) #$(%current-target-system) file)))
                       (chmod target-file #o777)))
                   '("gcc" "g++")))))
            ;; (add-after 'fix-env 'xyz
            ;;   (lambda _
            ;;     (exit 1)))
            (add-before 'build 'fix-target-detection
              (lambda _
                ;; NSIS target detection is screwed up, manually change
                ;; it ourselves
                (substitute* "Source/build.cpp" (("m_target_type=TARGET_X86UNICODE")
                                                 (string-append "m_target_type=" #$nsis-target-type))))))))
      (home-page "https://nsis.sourceforge.io/Main_Page")
      (synopsis "System to create Windows installers")
      (description
       "NSIS (Nullsoft Scriptable Install System) is a system to create
Windows installers.  It is designed to be as small and flexible as possible
and is therefore very suitable for internet distribution.")
      (license (license:non-copyleft "file://COPYING"
                                     "See COPYING in the distribution.")))))

(define-public nsis-x86_64
  (make-nsis "x86_64" "amd64" "TARGET_AMD64"))

(define-public nsis-i686
  (make-nsis "i686" "x86" "TARGET_X86UNICODE"))
