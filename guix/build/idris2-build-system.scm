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

(define-module (guix build idris2-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases idris2-build))

(define (invoke-idris2 command ipkg-name)
  (invoke "idris2" command (string-append ipkg-name ".ipkg")))

(define* (set-idris2-prefix #:key outputs #:allow-other-keys)
  (let ((prefix-dir (string-append (assoc-ref outputs "out") "/lib")))
    (mkdir-p prefix-dir)
    (setenv "IDRIS2_PREFIX" prefix-dir)))

(define* (build #:key ipkg-name #:allow-other-keys)
  (invoke-idris2 "--build" ipkg-name))

(define* (install #:key ipkg-name install-source? #:allow-other-keys)
  (invoke-idris2 (if install-source? "--install-with-src" "--install") ipkg-name)
  ;; TODO: install executables correctly
  )

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'set-idris2-prefix set-idris2-prefix)
    (replace 'build build)
    (delete 'check)
    (replace 'install install)))

(define* (idris2-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Idris2 package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
