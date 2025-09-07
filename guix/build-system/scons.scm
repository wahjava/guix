;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system scons)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:export (%scons-build-system-modules
            scons-build
            scons-cross-build
            scons-build-system))

;; Commentary:
;;
;; Standard build procedure for applications using SCons. This is implemented
;; as an extension of 'gnu-build-system'.
;;
;; Code:

(define %scons-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build scons-build-system)
    ,@%default-gnu-imported-modules))

(define (default-scons)
  "Return the default SCons package, resolved lazily."
  (@* (gnu packages build-tools) scons))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (implicit-inputs? #t) (implicit-cross-inputs? #t)
                (scons (default-scons))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:scons #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (target target)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs

                   ))
    (build-inputs `(("scons" ,scons)
                    ,@native-inputs

                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(if implicit-inputs?
                          (standard-packages system)
                          '())
                    ,@(if (and target implicit-cross-inputs?)
                          (standard-cross-packages target 'host)
                          '())))
    (target-inputs (if (and target implicit-cross-inputs?)
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target scons-cross-build scons-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (scons-build name inputs
                      #:key
                      (source #f)
                      (tests? #t)
                      (scons-flags ''())
                      (build-targets #~'())
                      (test-target "test")
                      (install-targets #~'("install"))
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %scons-build-system-modules)
                      (modules '((guix build scons-build-system)
                                 (guix build utils))))
  "Build SOURCE using SCons, and with INPUTS.  This assumes that SOURCE
provides a 'SConstruct' file as its build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(scons-build #:name #$name
                             #:source #+source
                             #:scons-flags #$(if (pair? scons-flags)
                                                 (sexp->gexp scons-flags)
                                                 scons-flags)
                             #:system #$system
                             #:build-targets #$build-targets
                             #:test-target #$test-target
                             #:tests? #$tests?
                             #:install-targets #$install-targets
                             #:phases #$(if (pair? phases)
                                            (sexp->gexp phases)
                                            phases)
                             #:outputs %outputs
                             #:inputs %build-inputs
                             #:search-paths
                             '#$(sexp->gexp
                                 (map search-path-specification->sexp
                                      search-paths)))))))

  (gexp->derivation name builder
                    #:system system
                    #:target #f
                    #:graft? #f
                    #:guile-for-build guile))

(define* (scons-cross-build name
                      #:key
                      build-inputs target-inputs host-inputs
                      (source #f)
                      (tests? #f)
                      (scons-flags ''())
                      (build-targets #~'())
                      (test-target "test")
                      (install-targets #~'("install"))
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (native-search-paths '())
                      (system (%current-system))
                      (target #f)
                      (build (nix-system->gnu-triplet system))
                      (guile #f)
                      (imported-modules %scons-build-system-modules)
                      (modules '((guix build scons-build-system)
                                 (guix build utils))))
  "Build SOURCE using SCons, and with INPUTS.  This assumes that SOURCE
provides a 'SConstruct' file as its build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))

          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs
            #$(outputs->gexp outputs))

          (define %output
            (assoc-ref %outputs "out"))

          (setenv "CC" #$(cc-for-target target))
          (setenv "CXX" #$(cxx-for-target target))
          (setenv "AR" #$(ar-for-target target))
          (setenv "LD" #$(ld-for-target target))
          (setenv "STRIP" #$(strip-for-target target))
          (when (false-if-exception
                 (search-input-file %build-host-inputs
                                    (string-append "/bin/" #$(pkg-config-for-target target))))
            (setenv "PKG_CONFIG" #$(pkg-config-for-target target)))

          (scons-build #:name #$name
                       #:source #+source
                       #:scons-flags #$(if (pair? scons-flags)
                                           (sexp->gexp scons-flags)
                                           scons-flags)
                       #:system #$system
                       #:target #$target
                       #:build-targets #$build-targets
                       #:test-target #$test-target
                       #:tests? #$tests?
                       #:install-targets #$install-targets
                       #:phases #$(if (pair? phases)
                                      (sexp->gexp phases)
                                      phases)
                       #:outputs %outputs
                       #:inputs %build-target-inputs
                       #:native-inputs %build-host-inputs
                       #:build #$build
                       #:native-search-paths '#$(sexp->gexp
                                                 (map
                                                  search-path-specification->sexp
                                                  native-search-paths))
                       #:search-paths
                       '#$(sexp->gexp
                           (map search-path-specification->sexp
                                search-paths))))))

  (gexp->derivation name builder
                    #:system system
                    #:target target
                    #:graft? #f
                    #:guile-for-build guile))

(define scons-build-system
  (build-system
    (name 'scons)
    (description "The standard SCons build system")
    (lower lower)))

;;; scons.scm ends here
