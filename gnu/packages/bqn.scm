;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Christopher Rodriguez <yewscion@gmail.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; © 2025 λx.x <case_lambda@disroot.org>
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

(define-module (gnu packages bqn)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public dbqn
  (package
    (name "dbqn")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dzaima/BQN")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06mzvv7kmandhgwb6jwz3rivsj4ic549sy8afnb5zr6mfn5isyg5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules `((guix build gnu-build-system)
                  ((guix build ant-build-system)
                   #:prefix ant:)
                  (guix build utils))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (replace 'build
                     (lambda* _
                       (invoke "./build")))
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (invoke "./BQN" "test/test"))))
                   (add-after 'install 'reorder-jar-content
                     (lambda* (#:key outputs #:allow-other-keys)
                       ((assoc-ref ant:%standard-phases
                                   'reorder-jar-content)
                        #:outputs outputs)))
                   (add-after 'reorder-jar-content 'jar-indices
                     (lambda* (#:key outputs #:allow-other-keys)
                       ((assoc-ref ant:%standard-phases
                                   'generate-jar-indices)
                        #:outputs outputs)))
                   (add-after 'jar-indices 'fix-jar-timestamps
                     (lambda* (#:key outputs #:allow-other-keys)
                       ((assoc-ref ant:%standard-phases
                                   'reorder-jar-content)
                        #:outputs outputs)))
                   (replace 'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (dest-bin (string-append out "/bin"))
                              (dest-jar (string-append out "/share/java")))
                         (rename-file "BQN" "dbqn")
                         (install-file "dbqn" dest-bin)
                         (install-file "BQN.jar" dest-jar)
                         (substitute* (string-append dest-bin "/dbqn")
                           (("BQN.jar")
                            (string-append dest-jar "/BQN.jar")))))))))
    (native-inputs (list `(,icedtea-8 "jdk") zip))
    (inputs (list icedtea-8 bash-minimal))
    (synopsis "BQN implementation based on dzaima/APL")
    (description
     "dbqn is a Java implementation of the
@uref{https://mlochbaum.github.io/BQN/, BQN programming language} that does
not need to be bootstrapped, based on an earlier Java implementation of APL by
the same author.")
    (home-page "https://github.com/dzaima/BQN")
    (license license:expat)))

(define bqn-sources
  ;; Aside from dbqn above, the main bqn repository is used by other
  ;; implementations as a "known good" set of sources. CBQN uses dbqn to
  ;; generate an intermediate bytecode for its own compilation.
    (let ((commit "d54d38e7297688e240fada0a99d62a9ae9fa4465"))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mlochbaum/BQN")
              (commit commit)))
        (file-name (git-file-name "bqn-sources" commit))
        (sha256
         (base32 "0av010c2syd69kbs65cblsfzawfg4w9pkllz4gs41yfgns228mx1")))))

(define cbqn-bootstrap
  (package
    (name "cbqn-bootstrap")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dzaima/CBQN")
              (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0433hp9lgv6w6mhdz0k1kx2rmxia76yy9i0z7ps4qdk7snf2yr2q"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; skipping tests for bootstrap
      #:make-flags
      #~(list "REPLXX=0" "nogit=1" "singeli=0" ;;TODO: build with REPLXX and Singeli
              (string-append "CC="
                             #$(cc-for-target))
              (string-append "version=v"
                             #$version))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'generate-bytecode
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "build/bytecodeLocal/gen")
              (invoke (search-input-file inputs "/bin/dbqn")
                      "build/genRuntime"
                      #+bqn-sources "build/bytecodeLocal")))
          (replace 'install
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (rename-file "BQN" "bqn")
              (install-file "bqn"
                            (string-append #$output "/bin")))))))
    (native-inputs (list dbqn bqn-sources))
    (inputs (list icedtea-8 libffi))
    (synopsis "BQN implementation in C")
    (description "This package provides the reference implementation of
@uref{https://mlochbaum.github.io/BQN/, BQN}, a programming language inspired
by APL.")
    (home-page "https://mlochbaum.github.io/BQN/")
    (license license:gpl3)))

(define-public cbqn
  (package
    (inherit cbqn-bootstrap)
    (name "cbqn")
    (outputs '("out" "lib"))
    (arguments
     (substitute-keyword-arguments (package-arguments cbqn-bootstrap)
       ((#:tests? _) #t)
       ((#:make-flags flags)
        #~(cons* "shared-o3" "o3"
                 #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'check
              (lambda* (#:key inputs tests? #:allow-other-keys)
                (when tests?
                  (invoke "./BQN" "-M" "1000"
                          (search-input-file inputs "/test/this.bqn"))
                  (map (lambda (x)
                         (invoke "./BQN"
                                 (string-append "test/" x ".bqn")))
                       '("cmp" "equal" "copy" "random"))
                  (invoke "make" "-C" "test/ffi"))))
            (replace 'install
              (lambda _
                (rename-file "BQN" "bqn")
                (install-file "bqn"
                              (string-append #$output "/bin"))
                (install-file "libcbqn.so"
                              (string-append #$output:lib "/lib"))
                (install-file "include/bqnffi.h"
                              (string-append #$output:lib "/include"))))))))
    (native-inputs (list dbqn bqn-sources libffi))
    (properties `((tunable? . #t)))))
