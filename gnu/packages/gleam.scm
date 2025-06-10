;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages gleam)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages erlang))

(define-public gleam
  (package
    (name "gleam")
    (version "v1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gleam-lang/gleam.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rdbajwkx5d57cw7vvsafk6cp9pgdnydqsfmqnwkzx20qkscvl34"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (install-file "target/release/gleam"
                            (string-append #$output "/bin")))))))
    (inputs (cons* erlang
                   (cargo-inputs 'gleam)))
    (synopsis "Compiler for Gleam, a type-safe functional language")
    (description
     "Gleam is a type-safe, impure functional programming language that compiles to Erlang or Javascript.")
    (license license:asl2.0)
    (home-page "https://gleam.run/")))
