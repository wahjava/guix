;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 jgart <jgart@dismail.de>
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

(define-module (guix build-system idris2)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%idris2-build-system-modules
            %default-modules
            idris2-build-system))

(define (default-idris2)
  ;; Lazily resolve the binding to avoid a circular dependency
  (let ((idris (resolve-interface '(gnu packages idris2))))
    (module-ref idris2 'idris2)))

(define %idris2-build-system-modules
  `((guix build idris2-build-system)
    ,@%default-gnu-imported-modules))

(define %default-modules
  '((guix build idris2-build-system)
    (guix build utils)))

(define* (idris2-build name inputs
                       #:key
                       source
                       (phases '%standard-phases)
                       ipkg-name
                       (install-source? #t)
                       (outputs '("out"))
                       (search-paths '())
                       (system (%current-system))
                       (guile #f)
                       (imported-modules %idris2-build-system-modules)
                       (modules %default-modules))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (idris2-build #:name #$name
                        #:source #+source
                        #:system #$system
                        #:ipkg-name #$ipkg-name
                        #:install-source? #$install-source?
                        #:phases #$(if (pair? phases)
                                       (sexp->gexp phases)
                                       phases)
                        #:outputs #$(outputs->gexp outputs)
                        #:inputs #$(input-tuples->gexp inputs)
                        #:search-paths '#$(sexp->gexp
                                           (map search-path-specification->sexp
                                                search-paths))))))


  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      ;; #:target #f
                      #:guile-for-build guile)))

;; TODO: transitive dependencies
(define (expand-idris2-inputs inputs)
  (filter-map
   (match-lambda
     ((label (? package? p))
      (list label (package-source p)))
     ((label input)
      (list label input)))
   inputs))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (idris2 (default-idris2))
                (idris2-inputs '())
                #:allow-other-keys
                #:rest arguments)
  ;; NOTE: derived from (gnu build-system cargo)
  ;;
  (define private-keywords
    '(#:inputs #:native-inputs #:outputs
      #:idris2 #:idris2-inputs
      #:target
      ;; ,@(if target '() '(#:target))
      ))

  ;; TODO: cross-compilation support
  (and (not target)
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@(expand-idris2-inputs idris-inputs)
                        ,@(if target inputs '())))
         (build-inputs `(("idris2" ,idris2)
                         ,@native-inputs
                         ,@(if target '() inputs)
                         ,@(if target
                               (standard-cross-packages target 'host)
                               '())))
         (target-inputs `(,@(if target
                                (standard-cross-packages target 'target)
                                '())))
         (outputs outputs)
         (build idris2-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define idris2-build-system
  (build-system
    (name 'idris2)
    (description "Idris2 build system")
    (lower lower)))
