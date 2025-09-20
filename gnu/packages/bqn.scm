;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Christopher Rodriguez <yewscion@gmail.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression))

(define-public dbqn
  (package
    (name "dbqn")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dzaima/BQN")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zy3y9wbmaw0mrd2sp7d1r912gvs9k0mzw5d3drgmbzkbvpd6iq1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
      #:modules `((guix build gnu-build-system)
                  ((guix build ant-build-system)
                   #:prefix ant:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (invoke "./build")))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests?
                  (invoke "./BQN" "./test/test")
                  (begin
                    (display "test suite not run")
                    (newline)))))
          (add-after 'install 'reorder-jar-content
            (assoc-ref ant:%standard-phases
                       'reorder-jar-content))
          (add-after 'reorder-jar-content 'generate-jar-indices
            (assoc-ref ant:%standard-phases
                       'generate-jar-indices))
          (add-after 'generate-jar-indices 'strip-jar-timestamps
            (assoc-ref ant:%standard-phases
                       'strip-jar-timestamps))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((dest-jar (string-append #$output "/share/java")))
                (rename-file "BQN" "dbqn")
                (substitute* "dbqn"
                  (("java")
                   (search-input-file inputs "/bin/java"))
                  (("BQN.jar")
                   (string-append dest-jar "/BQN.jar")))
                (install-file "BQN.jar" dest-jar)
                (install-file "dbqn"
                              (string-append #$output "/bin"))))))))
    (native-inputs (list `(,icedtea-8 "jdk") zip))
    (inputs (list bash-minimal icedtea-8))
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
    (let ((commit "71ce36141aaacfa714edca2e408ca522a3bc5554"))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/mlochbaum/BQN")
              (commit commit)))
        (file-name (git-file-name "bqn-sources" commit))
        (sha256
         (base32 "060a3r5m7hynzxj4iz1av2kj5jf8w3j8yswzzx9wkx31rdrsiv2c")))))

(define cbqn-bootstrap
  (let* ((revision "2")
         (commit "66584ce1491d300746963b8ed17170348b2a03e6"))
    (package
      (name "cbqn-bootstrap")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dzaima/CBQN")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13gg96aa56b8k08bjvv8i0f5nxrah2sij7g6pg7i21fdv08rd9iv"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; skipping tests for bootstrap
        #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'build 'generate-bytecode
              (lambda* (#:key inputs #:allow-other-keys)
                (system (string-append #+dbqn
                                       "/bin/dbqn ./genRuntime "
                                       #+bqn-sources))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (mkdir-p (string-append #$output "/bin"))
                (chmod "BQN" #o755)
                (rename-file "BQN" "bqn")
                (install-file "bqn" (string-append #$output "/bin")))))))
      (native-inputs (list dbqn bqn-sources))
      (inputs (list icedtea-8 libffi))
      (synopsis "BQN implementation in C")
      (description "This package provides the reference implementation of
@uref{https://mlochbaum.github.io/BQN/, BQN}, a programming language inspired
by APL.")
      (home-page "https://mlochbaum.github.io/BQN/")
      (license license:gpl3))))

(define-public cbqn
  (package
    (inherit cbqn-bootstrap)
    (name "cbqn")
    (outputs '("out" "lib"))
    (arguments
     (substitute-keyword-arguments (strip-keyword-arguments
                                    (list #:tests?)
                                    (package-arguments cbqn-bootstrap))
       ((#:make-flags flags #~(list))
        #~(cons* "shared-o3" "o3" #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (replace 'check
              (lambda* (#:key inputs tests? #:allow-other-keys)
                (when tests?
                  (system (string-append "./BQN -M 1000 \""
                                         #+bqn-sources
                                         "/test/this.bqn\""))
                  (map (lambda (x)
                         (system (string-append "./BQN ./test/" x
                                                ".bqn")))
                       '("cmp" "equal" "copy" "random"))
                  (system "make -C test/ffi"))))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((bin (string-append (assoc-ref outputs "out")
                                           "/bin"))
                       (lib (string-append (assoc-ref outputs "lib")
                                           "/lib"))
                       (include (string-append (assoc-ref outputs "lib")
                                           "/include")))
                  (mkdir-p bin)
                  (rename-file "BQN" "bqn")
                  (install-file "bqn" bin)
                  (install-file "libcbqn.so" lib)
                  (install-file "include/bqnffi.h" include))))))))
    (native-inputs (list dbqn
                         bqn-sources
                         libffi))
    (properties
     `((tunable? . #t)))))
