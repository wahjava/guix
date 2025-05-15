;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Jørgen Kvalsvik <j@lambda.is>
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

(define-module (guix build-system go-module)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module ((guix build-system go) #:prefix go-build:)
  #:use-module (srfi srfi-1)
  #:export (%go-module-build-system-modules
            go-module-build
            go-module-build-system))

;;; Commentary:
;;;
;;; Build procedure for packages using the module aware Go build system.
;;;
;;; Code:

(define %go-module-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build go-module-build-system)
    (guix build union)
    ,@%default-gnu-imported-modules))

(define (default-go)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((go (resolve-interface '(gnu packages golang))))
    (module-ref go 'go)))

(define (default-gccgo)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((gcc (resolve-interface '(gnu packages gcc))))
    (module-ref gcc 'gccgo-12)))

(define (default-zip)
  "Return the 'zip' package.  This is a lazy reference so that we don't
depend on (gnu packages compression)."
  (let ((distro (resolve-interface '(gnu packages compression))))
    (module-ref distro 'zip)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (if (supported-package? (default-go))
                      (default-go)
                      (default-gccgo)))
                (zip (default-zip))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:inputs #:native-inputs #:go #:zip))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source `(("source" ,source)) '())
                    ,@`(("go" ,go) ("zip" ,zip))
                    ,@native-inputs
                    ,@(if target '() inputs)
                    ,@(if target (standard-cross-packages target 'host) '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs (if target inputs '()))
    (target-inputs (if target (standard-cross-packages target 'target) '()))
    (outputs outputs)
    (build (if target go-cross-module-build go-module-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (go-module-build name inputs
                          #:key
                          source
                          (phases '%standard-phases)
                          (outputs '("out"))
                          (search-paths '())
                          (go-flags '())
                          (ld-flags '("-s" "-w"))
                          (tags '())
                          (build-targets '("./..."))
                          (test-targets '("./..."))
                          (install-targets '())
                          (test-flags '())
                          (module-path #f)
                          (trimpath? #t)
                          (cgo? #f)
                          (tests? #t)
                          (build-output-dir? #f)
                          (skip-build? #f)
                          (install-source? #t)
                          (install-cache? #t)
                          (parallel-build? #t)
                          (parallel-tests? #t)
                          (environment-variables '())
                          (system (%current-system))
                          (goarch #f)
                          (goos #f)
                          (guile #f)
                          (substitutable? #t)
                          (imported-modules %go-module-build-system-modules)
                          (modules '((guix build go-module-build-system)
                                     (guix build utils))))

  (define builder
    (with-imported-modules
     imported-modules
     #~(begin
         (use-modules #$@(sexp->gexp modules))
         (go-module-build #:name #$name
                          #:source #+source
                          #:system #$system
                          #:go-flags '#$go-flags
                          #:ld-flags '#$ld-flags
                          #:tags '#$tags
                          #:build-targets '#$build-targets
                          #:test-targets '#$test-targets
                          #:install-targets '#$install-targets
                          #:test-flags '#$test-flags
                          #:module-path '#$module-path
                          #:trimpath? #$trimpath?
                          #:cgo? '#$cgo?
                          #:tests? #$tests?
                          #:build-output-dir? #$build-output-dir?
                          #:skip-build? #$skip-build?
                          #:install-source? #$install-source?
                          #:install-cache? #$install-cache?
                          #:parallel-build? #$parallel-build?
                          #:parallel-tests? #$parallel-tests?
                          #:environment-variables '#$environment-variables
                          #:goarch #$goarch
                          #:goos #$goos
                          #:phases #$phases
                          #:outputs #$(outputs->gexp outputs)
                          #:search-paths '#$(map
                                             search-path-specification->sexp
                                             search-paths)
                          #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
        (gexp->derivation name builder
                          #:system system
                          #:guile-for-build guile)))

(define* (go-cross-module-build name
                                #:key
                                source target
                                build-inputs target-inputs host-inputs
                                (phases '%standard-phases)
                                (outputs '("out"))
                                (search-paths '())
                                (native-search-paths '())
                                (go-flags '())
                                (ld-flags '("-s" "-w"))
                                (tags '())
                                (build-targets '("./..."))
                                (test-targets '())
                                (install-targets '())
                                (tests? #f)              ; nothing can be done
                                (test-flags '())
                                (module-path #f)
                                (trimpath? #t)
                                (cgo? #f)
                                (build-output-dir? #f)
                                (skip-build? #f)
                                (install-source? #t)
                                (install-cache? #t)
                                (parallel-build? #t)
                                (parallel-tests? #f)
                                (environment-variables '())
                                (system (%current-system))
                                (goarch (if target (first (go-build:go-target target)) #f))
                                (goos (if target (last (go-build:go-target target)) #f))
                                (guile #f)
                                (imported-modules %go-module-build-system-modules)
                                (modules '((guix build go-module-build-system)
                                           (guix build utils)))
                                (substitutable? #t))

  (define builder
    (with-imported-modules
     imported-modules
     #~(begin
         (use-modules #$@(sexp->gexp modules))

         (define %build-host-inputs
           #+(input-tuples->gexp build-inputs))

         (define %build-target-inputs
           (append #$(input-tuples->gexp host-inputs)
                   #+(input-tuples->gexp target-inputs)))

         (define %build-inputs
           (append %build-host-inputs %build-target-inputs))

         (go-module-build #:name #$name
                          #:source #+source
                          #:system #$system
                          #:go-flags '#$go-flags
                          #:ld-flags '#$ld-flags
                          #:tags '#$tags
                          #:build-targets '#$build-targets
                          #:test-targets '#$test-targets
                          #:install-targets '#$install-targets
                          #:test-flags '#$test-flags
                          #:module-path '#$module-path
                          #:trimpath? #$trimpath?
                          #:cgo? '#$cgo?
                          #:tests? #$tests?
                          #:build-output-dir? #$build-output-dir?
                          #:skip-build? #$skip-build?
                          #:install-source? #$install-source?
                          #:install-cache? #$install-cache?
                          #:parallel-build? #$parallel-build?
                          #:parallel-tests? #$parallel-tests?
                          #:environment-variables '#$environment-variables
                          #:target #$target
                          #:goarch #$goarch
                          #:goos #$goos
                          #:phases #$phases
                          #:outputs #$(outputs->gexp outputs)
                          #:make-dynamic-linker-cache? #f
                          #:search-paths '#$(map
                                             search-path-specification->sexp
                                             search-paths)
                          #:native-search-paths '#$(map
                                                    search-path-specification->sexp
                                                    native-search-paths)
                          #:native-inputs %build-host-inputs
                          #:inputs %build-inputs))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
        (gexp->derivation name builder
                          #:system system
                          #:target target
                          #:graft? #f
                          #:substitutable? substitutable?
                          #:guile-for-build guile)))

(define go-module-build-system
  (build-system
   (name 'go-module)
   (description "Go Module Build System")
   (lower lower)))

;;; go-module.scm ends here
