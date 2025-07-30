;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages perl-maths)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages xorg))

(define-public perl-math-cephes
  (package
    (name "perl-math-cephes")
    (version "0.5305")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SH/SHLOMIF/Math-Cephes-"
                    version ".tar.gz"))
              (sha256
               (base32
                "18c3xg53d1vv7hlj43601jj7ks119fm6ndpwpv94irr2905806jn"))
              ;; For reproducibility
              (modules '((guix build utils)))
              (snippet
               '(substitute* "libmd/Makefile.PL"
                  (("readdir DIR") "sort readdir DIR")))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Math-Cephes")
    (synopsis "Perl interface to the Cephes math library")
    (description "The Math::Cephes module provides a Perl interface to over
150 functions of the Cephes math library.")
    (license license:perl-license)))

(define-public perl-math-matrixreal
  (package
    (name "perl-math-matrixreal")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/L/LE/LETO/Math-MatrixReal-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1cml5wqd99hm398gl8f147ccsck9v179l7a6vqjj4kfkdnja37sg"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-test-most))
    (home-page "https://metacpan.org/release/Math-MatrixReal")
    (synopsis "Manipulate NxN matrices of real numbers")
    (description "This package provides the @code{Math::MatrixReal} module.
It implements the data type \"matrix of real numbers\" (and consequently also
\"vector of real numbers\").")
    (license license:perl-license)))

(define-public perl-math-interpolate
  (package
    (name "perl-math-interpolate")
    (version "1.06")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BZ/BZAJAC/Math-Interpolate-" version
             ".tar.gz"))
       (sha256
        (base32 "0snfrg9vk0f9bznpv274f21p65gxv08x4m1myg2l5k4yvyzjyl54"))))
    (build-system perl-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda _
              ;; perl doesn't like the original format.
              (substitute* "Makefile.PL"
               (("'MIN_PERL_VERSION'[ \t]*=>[ \t]*'5.004_01'")
                "'MIN_PERL_VERSION' => '5.005'")))))))
    (home-page "https://metacpan.org/release/Math-Interpolate")
    (synopsis "Interpolate the value Y from X using a list of (X, Y) pairs")
    (description "This packages allows you to interpolate the value Y from X
using a list of (X, Y) pairs.")
    (license license:perl-license)))

;; This is NOT compatible with perl-pdl-2.095
(define-public perl-pdl-2.019
  (package
    (name "perl-pdl")
    (version "2.019")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cpan.org/modules/by-module/PDL/PDL-" version
                           ".tar.gz"))
       (sha256
        (base32 "1zkp5pm351qjr6sb3xivpxy5vjfl72ns0hb4dx5vpm8xvgp7p92i"))
       (patches
        (search-patches "pdl-2.019-glut-bitmap-fonts.patch"))))
    (build-system perl-build-system)
    (arguments
     `(#:make-maker-flags
       ;; This is required because of "gsl_set_error_handler_off".
       `("OPTIMIZE=-O2 -Wno-error=implicit-function-declaration")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-includes
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "perldl.conf"
              (("WITH_BADVAL => 0") "WITH_BADVAL => 1")
              (("WITH_GD => undef") "WITH_GD => true")
              (("GD_INC => undef")
               (string-append "GD_INC => '" (assoc-ref inputs "gd") "/include'"))
              (("GD_LIB => undef")
               (string-append "GD_LIB => '" (assoc-ref inputs "gd") "/lib'"))
              (("WITH_GSL => undef") "WITH_GSL => true"))))
         (add-before 'check 'prepare-check
           (lambda _
             ;; Set this for official CPAN releases of PDL.
             (setenv "SKIP_KNOWN_PROBLEMS" "1")
             (setenv "HOME" "/tmp"))))))
    (inputs (list gd gsl mesa glu))
    (native-inputs (list perl-devel-checklib
                         perl-extutils-depends
                         perl-extutils-f77
                         gfortran
                         sharutils
                         perl-file-which
                         perl-pod-parser
                         perl-test-deep
                         perl-test-exception
                         perl-test-warn))
    (propagated-inputs (list perl-file-map
                             perl-file-which
                             perl-inline
                             perl-inline-c
                             perl-list-moreutils
                             perl-opengl
                             perl-pod-parser
                             perl-sys-sigaction
                             perl-termreadkey))
    (home-page "https://metacpan.org/release/PDL")
    (synopsis "Perl Data Language")
    (description "This package provides a library and simple REPL for the
Perl Data Language.")
    (license license:perl-license)))

;; The project has split out several packages.
(define-public perl-pdl
  (package
    (inherit perl-pdl-2.019)
    (name "perl-pdl")
    (version "2.100")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PDLPorters/pdl.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nkvjb4l8r2l2piz0s9jifpm0kr6g7fy9krky130s82vq8bz19sc"))
       (patches
        (search-patches "pdl-2.100-reproducibility.patch"))))
    (build-system perl-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'patch-includes
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/PDL/Core/Dev.pm"
              ;; This would cause an underflow (1 - 120 < 0)--so prevent it.
              (("utime [$]mtime - 120, [$]mtime - 120, [$]pmfile; # so is out of date")
               "utime $mtime - 1, $mtime - 1, $pmfile; # so is out of date")))))))
    (inputs (list gd gsl mesa glu))
    (native-inputs (list perl-capture-tiny ; for a test
                         perl-devel-checklib
                         perl-extutils-depends
                         perl-extutils-f77
                         gfortran
                         perl-file-which
                         perl-pod-parser
                         perl-test-deep
                         perl-test-exception
                         perl-test-warn
                         perl-text-balanced)) ; our perl is too old, so...
    (propagated-inputs (list perl-file-map
                             perl-file-which
                             perl-inline
                             perl-inline-c
                             perl-list-moreutils
                             perl-opengl
                             perl-pod-parser
                             perl-sys-sigaction
                             perl-termreadkey))
    (home-page "https://metacpan.org/release/PDL")
    (synopsis "Perl Data Language")
    (description "This package provides a library and simple REPL for the
Perl Data Language.")
    (license license:perl-license)))

(define-public perl-pdl-graphics-trid
  (package
    (name "perl-pdl-graphics-trid")
    (version "2.102")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/PDLPorters/PDL-Graphics-TriD.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c66337l3dkw70mqgldbih1xkyg082lalmaldb9k5058hq4jhqzl"))))
    (build-system perl-build-system)
    (inputs
     (list mesa))
    (propagated-inputs
     (list perl-pdl))
    (synopsis "3D graphics in Perl Data Language")
    (description "This package provides modules to do 3D graphics in PDL.")
    (home-page "https://github.com/PDLPorters/")
    (license license:perl-license)))

(define-public perl-pgplot
  (package
    (name "perl-pgplot")
    (version "2.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PDLPorters/perl5-PGPLOT.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i9bi52pwbi3qnalm288ihdlwsvn9wwi5rhmspqbna3pfqjhc29c"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'setenv
         (lambda* (#:key inputs #:allow-other-keys)
           (setenv "PGPLOT_DIR" (string-append (assoc-ref inputs "giza") "/lib")))))))
    (inputs (list giza libx11))
    (native-inputs (list perl-devel-checklib perl-extutils-f77 gfortran perl-pdl))
    (home-page "https://metacpan.org/release/PGPLOT")
    (synopsis "Scientific plotting library (using giza)")
    (description "This package provides PGPLOT bindings for Perl.  It uses
giza instead of PGPLOT for the implementation, though.")
    ;; Since giza is GPL2+, so is this.
    (license license:gpl2+)))
