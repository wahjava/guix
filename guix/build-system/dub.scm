;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (guix build-system dub)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:export (dub-build-system))

;; Lazily resolve bindings to avoid a circular dependencies.
(define (default-ldc)
  (@* (gnu packages dlang) ldc))

(define (default-dub)
  (@* (gnu packages dlang) dub))

(define (default-pkg-config)
  (@* (gnu packages pkg-config) pkg-config))

(define (default-ld-gold-wrapper)
  ;; LDC doesn't work with Guix's default (BFD) linker.
  ;; Lazily resolve the binding to avoid a circular dependency.
  (@* (gnu packages commencement) ld-gold-wrapper))

(define %dub-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build dub-build-system)
    ,@%default-gnu-imported-modules))

(define* (dub-build name inputs
                    #:key
                    source
                    (tests? #t)
                    (test-target #f)
                    (dub-build-flags ''())
                    (phases '%standard-phases)
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules %dub-build-system-modules)
                    (modules '((guix build dub-build-system)
                               (guix build utils))))
  "Build SOURCE using DUB, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (dub-build #:name #$name
                     #:source #+source
                     #:system #$system
                     #:test-target #$test-target
                     #:dub-build-flags #$dub-build-flags
                     #:tests? #$tests?
                     #:phases #$phases
                     #:outputs #$(outputs->gexp outputs)
                     #:search-paths '#$(sexp->gexp
                                        (map search-path-specification->sexp
                                             search-paths))
                     #:inputs #$(input-tuples->gexp inputs)))))

  (mbegin %store-monad
    (return builder)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (ldc (default-ldc))
                (dub (default-dub))
                (pkg-config (default-pkg-config))
                (ld-gold-wrapper (default-ld-gold-wrapper))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:ldc #:dub #:pkg-config #:inputs #:native-inputs #:outputs))

  (and (not target) ;; TODO: support cross-compilation
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'
                        ,@(standard-packages)))
         (build-inputs `(("ldc" ,ldc)
                         ("dub" ,dub)
                         ("ld-gold-wrapper" ,ld-gold-wrapper)
                         ,@native-inputs))
         (outputs outputs)
         (build dub-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define dub-build-system
  (build-system
    (name 'dub)
    (description
     "DUB build system, to build D packages")
    (lower lower)))
