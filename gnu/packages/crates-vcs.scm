;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
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

(define-module (gnu packages crates-vcs)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sequoia)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public rust-git-testament-0.2
  (package
    (name "rust-git-testament")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "git-testament" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c9l10wpyz39vhb5cijvbym6gmpmw3y3nb35l2hg6w42h1ygaswq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-git-testament-derive" ,rust-git-testament-derive-0.1)
        ("rust-no-std-compat" ,rust-no-std-compat-0.4))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description "Record git working tree status when compiling your crate")
    (license license:bsd-3)))

(define-public rust-git-testament-derive-0.1
  (package
    (name "rust-git-testament-derive")
    (version "0.1.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "git-testament-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlais0i47mgsmp3r5jcqry2agjfyg5s9paj6mgvfykchssjsy2a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs
       (("rust-git-testament" ,rust-git-testament-0.2))))
    (home-page "https://github.com/kinnison/git-testament/")
    (synopsis "Record git working tree status when compiling your crate")
    (description
     "This package provides an inner procedural macro for git-testament.")
    (license license:bsd-3)))

(define-public rust-git-version-0.3
  (package
    (name "rust-git-version")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qj1rd19v8vg094b3fj0gy6ca53v93lhrl31wg1fs7g0y61qx4cl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-git-version-macro" ,rust-git-version-macro-0.3)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Embed git information in your code at compile-time")
    (description
     "This crates compiles the git version (tag name, or hash otherwise) and
dirty state into your program.")
    (license license:bsd-2)))

(define-public rust-git-version-macro-0.3
  (package
    (name "rust-git-version-macro")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-version-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mynlf8sfaa4xx7qff0qgnr339fbf1svgr569yip067fzm97ma9l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/fusion-engineering/rust-git-version")
    (synopsis "Internal macro crate for git-version")
    (description
     "This is an internal macro crate for git-version.")
    (license license:bsd-2)))

(define-public rust-git2-0.18
  (package
    (name "rust-git2")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kf0kvg3i7p1223zs2h9fz99ndm0l9kdx3hcw63g73dh5nlppygv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags (list "--release" "--"
                                "--skip=cred::test::credential_helper5")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libgit2-sys" ,rust-libgit2-sys-0.16)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-structopt" ,rust-structopt-0.3)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Rust bindings to libgit2")
    (description
     "This package provides bindings to libgit2 for interoperating with git
repositories.  This library is both threadsafe and memory safe and allows both
reading and writing git repositories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-0.17
  (package
    (inherit rust-git2-0.18)
    (name "rust-git2")
    (version "0.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i00kg3yizh7mn6hnj3yz3hpniisidlavifgy8n3cnm9gim9v63v"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.15)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     (list pkg-config
           git-minimal))                ;for a single test
    (inputs (list libgit2-1.6 libssh2 openssl zlib))))

(define-public rust-git2-0.16
  (package
    (inherit rust-git2-0.17)
    (name "rust-git2")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k1aavsfhk0i9jycc9gb61w2jwy8w9dgkd7zkz295wwm566gdxyc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     (list pkg-config
           git-minimal))                ;for a single test
    (inputs
     (list libgit2 libssh2 openssl zlib))))

(define-public rust-git2-0.15
  (package
    (inherit rust-git2-0.16)
    (name "rust-git2")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lf7yxgrbglx5qqvk033n44ymmrg72z271911jwix9d6lgjbx519"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.14)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("git" ,git-minimal)))           ;for a single test
    (inputs
     (list libgit2 libssh2 openssl zlib))))

(define-public rust-git2-0.13
  (package
    (inherit rust-git2-0.15)
    (name "rust-git2")
    (version "0.13.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07rlxwvl5iyyqwh0mci5v27lbicf9qiqm60maw1srz7i51x00pl4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-time" ,rust-time-0.1))))
    (inputs
     (list libgit2-1.3 libssh2 openssl zlib))))

(define-public rust-git2-0.11
  (package
    (inherit rust-git2-0.13)
    (name "rust-git2")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i0fgsr91r97hsjbgqnymkcyiyg0057m7m04116k3vmyqpvrwlbp"))))
    (arguments
     `(#:tests? #f      ; (signal: 11, SIGSEGV: invalid memory reference)
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thread-id" ,rust-thread-id-3)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-git2-0.9
  (package
    (inherit rust-git2-0.11)
    (name "rust-git2")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09lj6i26yial0drdbmfh36avz6wizaxqb0k41sqn2kca1qv01d4c"))))
    (arguments
     `(#:tests? #f      ; Needs older version of libgit2.
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-thread-id" ,rust-thread-id-3)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-git2-0.6
  (package
    (inherit rust-git2-0.11)
    (name "rust-git2-6")
    (version "0.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "115ys6vlfjy5pcwkip0wfzi4q3d1kimbl9isxvjyci1arnvlnnzf"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.6)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-url" ,rust-url-1))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-time" ,rust-time-0.1))))
    (inputs
     (modify-inputs (package-inputs rust-git2-0.11)
       (prepend curl)))))

(define-public rust-git2-curl-0.19
  (package
    (name "rust-git2-curl")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hzp64h1x8kr2vvf3bx195s1999sh8d0cygw4vykymwcc1hnpqkq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs (("rust-curl" ,rust-curl-0.4)
                       ("rust-git2" ,rust-git2-0.18)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Libgit2 HTTP transport backend powered by @code{libcurl}")
    (description "Backend for an HTTP transport in @code{libgit2}, powered by
libcurl, which is intended to be used with the @code{git2} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-curl-0.18
  (package
    (inherit rust-git2-curl-0.19)
    (name "rust-git2-curl")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "132zzrrfw3cnfh9ffc9pfr94my97agnmk7pnfvzqr4kj5d1vgy7q"))))
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs
       (("rust-curl" ,rust-curl-0.4)
        ("rust-git2" ,rust-git2-0.17)
        ("rust-log" ,rust-log-0.4)
        ("rust-url" ,rust-url-2))))))

(define-public rust-git2-curl-0.17
  (package
    (inherit rust-git2-curl-0.18)
    (name "rust-git2-curl")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git2-curl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cydakv91acxkxfj0kazp9sc4mmr8l51649mi06wk9qv6kkg8xvm"))))
    (arguments
     `(#:skip-build? #t                 ;need rust-civet and others
       #:cargo-inputs
       (("rust-curl" ,rust-curl-0.4)
        ("rust-git2" ,rust-git2-0.16)
        ("rust-log" ,rust-log-0.4)
        ("rust-url" ,rust-url-2))))))

(define-public rust-gix-0.55
  (package
    (name "rust-gix")
    (version "0.55.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1spf1hgpbn76y2am0q4i1qxwy8987g9f7byhs09r6y5v3v6nf9h0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-archive" ,rust-gix-archive-0.6)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-config" ,rust-gix-config-0.31)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.37)
                       ("rust-gix-discover" ,rust-gix-discover-0.26)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-filter" ,rust-gix-filter-0.6)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.20)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.9)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-odb" ,rust-gix-odb-0.54)
                       ("rust-gix-pack" ,rust-gix-pack-0.44)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.4)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.41)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.19)
                       ("rust-gix-revision" ,rust-gix-revision-0.23)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.2)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.5)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.38)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.27)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.4)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.6)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Interact with Git repositories using a pure Rust implementation")
    (description "Gitoxide is a pure Rust implementation of Git that aims to be lean
and correct.  Interact with Git repositories just like Git would.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-0.54
  (package
    (inherit rust-gix-0.55)
    (name "rust-gix")
    (version "0.54.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ly919iq2jnb28fvac5iwqv9jqn1pr7fl63xsn89cial8kkk4vdd"))))
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-archive" ,rust-gix-archive-0.5)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-config" ,rust-gix-config-0.30)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.20)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-diff" ,rust-gix-diff-0.36)
                       ("rust-gix-discover" ,rust-gix-discover-0.25)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.8)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-macros" ,rust-gix-macros-0.1)
                       ("rust-gix-mailmap" ,rust-gix-mailmap-0.19)
                       ("rust-gix-negotiate" ,rust-gix-negotiate-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-odb" ,rust-gix-odb-0.53)
                       ("rust-gix-pack" ,rust-gix-pack-0.43)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-protocol" ,rust-gix-protocol-0.40)
                       ("rust-gix-ref" ,rust-gix-ref-0.37)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.18)
                       ("rust-gix-revision" ,rust-gix-revision-0.22)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-status" ,rust-gix-status-0.1)
                       ("rust-gix-submodule" ,rust-gix-submodule-0.4)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-gix-transport" ,rust-gix-transport-0.37)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.26)
                       ("rust-gix-worktree-state" ,rust-gix-worktree-state-0.3)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-async-std" ,rust-async-std-1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-0.45
  (package
    (inherit rust-gix-0.54)
    (name "rust-gix")
    (version "0.45.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10ndy5kvczbgfpv7zjqh71hl3w8lin7p3az3pard297fcvn06amz"))))
    (arguments
     `(#:tests? #f      ; Tests aren't included
       #:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.21)
        ("rust-gix-attributes" ,rust-gix-attributes-0.13)
        ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.16)
        ("rust-gix-config" ,rust-gix-config-0.23)
        ("rust-gix-credentials" ,rust-gix-credentials-0.15)
        ("rust-gix-date" ,rust-gix-date-0.5)
        ("rust-gix-diff" ,rust-gix-diff-0.30)
        ("rust-gix-discover" ,rust-gix-discover-0.19)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-fs" ,rust-gix-fs-0.2)
        ("rust-gix-glob" ,rust-gix-glob-0.8)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-ignore" ,rust-gix-ignore-0.3)
        ("rust-gix-index" ,rust-gix-index-0.17)
        ("rust-gix-lock" ,rust-gix-lock-6)
        ("rust-gix-mailmap" ,rust-gix-mailmap-0.13)
        ("rust-gix-negotiate" ,rust-gix-negotiate-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-odb" ,rust-gix-odb-0.46)
        ("rust-gix-pack" ,rust-gix-pack-0.36)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-prompt" ,rust-gix-prompt-0.5)
        ("rust-gix-protocol" ,rust-gix-protocol-0.33)
        ("rust-gix-ref" ,rust-gix-ref-0.30)
        ("rust-gix-refspec" ,rust-gix-refspec-0.11)
        ("rust-gix-revision" ,rust-gix-revision-0.15)
        ("rust-gix-sec" ,rust-gix-sec-0.8)
        ("rust-gix-tempfile" ,rust-gix-tempfile-6)
        ("rust-gix-transport" ,rust-gix-transport-0.32)
        ("rust-gix-traverse" ,rust-gix-traverse-0.26)
        ("rust-gix-url" ,rust-gix-url-0.19)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-gix-validate" ,rust-gix-validate-0.7)
        ("rust-gix-worktree" ,rust-gix-worktree-0.18)
        ("rust-log" ,rust-log-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-prodash" ,rust-prodash-25)
        ("rust-regex" ,rust-regex-1)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-actor-0.28
  (package
    (name "rust-gix-actor")
    (version "0.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05xldn3aq58kjx2i87xsb2gdw7qhxvvikyvsiwvv85ppkq1cmb9f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, a way to identify Git actors")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package provides a way to identify Git actors.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-actor-0.27
  (package
    (inherit rust-gix-actor-0.28)
    (name "rust-gix-actor")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09sychqipf8bsmrwn62svpda457h8wj65qnl49qqk42j5jc0xih8"))))
    (arguments
     `(#:tests? #f  ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.8)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-actor-0.21
  (package
    (inherit rust-gix-actor-0.27)
    (name "rust-gix-actor")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-actor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f6q1ivdcgj26jln48nl2hphwmps6sb9m4dysldvzbz1dfgkzrwz"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.5)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-archive-0.5
  (package
    (name "rust-gix-archive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-archive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1izajfmninmjg7mvcp6y0mdy641hkvvqmsd7gp6d92mkgxgn6wdb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; undeclared crate gix_worktree, gix_filter, gix_odb
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree-stream" ,rust-gix-worktree-stream-0.5)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zip" ,rust-zip-0.6))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Archive generation of a Git worktree")
    (description "Archive generation of a worktree.  Part of Gitoxide a pure
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-attributes-0.19
  (package
    (name "rust-gix-attributes")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "086qgrh8srr5vyswbchn72kw967f25szjgk27dss96vhf1g6cl94"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-byteyarn" ,rust-byteyarn-0.2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crates deals with .gitattributes")
    (description
     "This package provides a crate from the Gitoxide project dealing with
.gitattributes files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-attributes-0.13
  (package
    (inherit rust-gix-attributes-0.19)
    (name "rust-gix-attributes")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-attributes" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cfciacamnqbxl02l5fb7pcfbzn6r9gnyh86gvc80brqmj89bdvq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-glob" ,rust-gix-glob-0.8)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-kstring" ,rust-kstring-2)
        ("rust-log" ,rust-log-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-bitmap-0.2
  (package
    (name "rust-gix-bitmap")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-bitmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n8r9n3rr6fkqggi99hgcqln4gnp8951pn3q3fsxsi38ayyb9jhc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implement the standard git bitmap format")
    (description
     "This package provides a crate of the gitoxide project dedicated
implementing the standard git bitmap format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-chunk-0.4
  (package
    (name "rust-gix-chunk")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-chunk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y2wp3clilp5rdl727x4q4qw1chbgq91l9spq6h7d07nmkgccgh0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Interact with the git chunk file format")
    (description
     "Interact with the git chunk file format used in multi-pack index and
commit-graph files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-command-0.2
  (package
    (name "rust-gix-command")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ix44maislxlranv67yw5fan5k82lpgax22zgc4jrxvpypxnqmrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Handle internal git command execution")
    (description
     "This package provides a crate of the gitoxide project handling internal git
command execution.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-commitgraph-0.22
  (package
    (name "rust-gix-commitgraph")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dpcdj9s5pkdvqpc22jm42y2lhkji2jgixps7a05kw11l1xh19w5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-memmap2" ,rust-memmap2-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Read only access to Git the commitgraph file format")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package provides read only access to git commitgraph file.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-commitgraph-0.21
  (package
    (inherit rust-gix-commitgraph-0.22)
    (name "rust-gix-commitgraph")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nzyh8gyg0k15swsy51m1iy521a7p5xbdg9pw1a1w1cw9g9fnp7"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-commitgraph-0.16
  (package
    (inherit rust-gix-commitgraph-0.21)
    (name "rust-gix-commitgraph")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-commitgraph" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "028s0ai44gb9ynic2ch3ingzg8h44c47q90xlzk4fp6mnghhljg8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-chunk" ,rust-gix-chunk-0.4)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-config-0.31
  (package
    (name "rust-gix-config")
    (version "0.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fzraij8rb98j71id939qc56nzaqfaqp8ln3kcvhjv66nk39ibjw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Git-config file parser and editor from the Gitoxide project")
    (description
     "A git-config file parser and editor from the Gitoxide project.  Gitoxide
is a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-0.30
  (package
    (inherit rust-gix-config-0.31)
    (name "rust-gix-config")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dcaz3ylf2x763mjfg7jd4qhx65c8l7zqzp3g9kkyza8815m2wf1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-config-value" ,rust-gix-config-value-0.14)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-gix-glob" ,rust-gix-glob-0.13)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-ref" ,rust-gix-ref-0.37)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2)
        ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5))))))

(define-public rust-gix-config-0.23
  (package
    (inherit rust-gix-config-0.30)
    (name "rust-gix-config")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0738kwxlmdp409rypczzr0ampbcvrrv2icigll68zfp118911wsi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-config-value" ,rust-gix-config-value-0.12)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-glob" ,rust-gix-glob-0.8)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-ref" ,rust-gix-ref-0.30)
        ("rust-gix-sec" ,rust-gix-sec-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-nom" ,rust-nom-7)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.4))))))

(define-public rust-gix-config-value-0.14
  (package
    (name "rust-gix-config-value")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03linqdbs6ci6ygj8qwmi0izz2ki55xypqyhi2g8y7ygyi3bxq2j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "This crate implements @code{git-config} value parsing")
    (description
     "This package is a crate for @code{git-config} value parsing.  Part of
Gitoxide a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-config-value-0.12
  (package
    (inherit rust-gix-config-value-0.14)
    (name "rust-gix-config-value")
    (version "0.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-config-value" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15rqyj523ckas16sn0jbqpgzln4h1fcpdsnwj4lw0hbl8d0lz1vf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-credentials-0.21
  (package
    (name "rust-gix-credentials")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1956pmz4sj25kydwh4ardzv9zbdpqrx050g5c4c2m14v0rs5sp0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Git credentials handlers for Gitoxide")
    (description
     "Gitoxide is a Rust implementation of Git.  This package provides helpers
to interact with Git credentials.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-credentials-0.20
  (package
    (inherit rust-gix-credentials-0.21)
    (name "rust-gix-credentials")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12mwq0fah6wai26lnq9k3m71lr8cgih43rqy2in6mby59j40p426"))))
    (arguments
     `(#:tests? #f ;wants undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-prompt" ,rust-gix-prompt-0.7)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-credentials-0.15
  (package
    (inherit rust-gix-credentials-0.20)
    (name "rust-gix-credentials")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-credentials" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r8dr9d1xzfq74sg3j2bhd6zsyk3y5322155izpzaa6dibm9zy66"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-command" ,rust-gix-command-0.2)
        ("rust-gix-config-value" ,rust-gix-config-value-0.12)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-prompt" ,rust-gix-prompt-0.5)
        ("rust-gix-sec" ,rust-gix-sec-0.8)
        ("rust-gix-url" ,rust-gix-url-0.19)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-date-0.8
  (package
    (name "rust-gix-date")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zh6yjxvzxbw92gz3f6846cbxk66wdjbwhp6nm4k9gmyfbxkszzv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crate parses dates the way Git does")
    (description
     "Part of Gitoxide, this crate parses dates the way git does.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-date-0.5
  (package
    (inherit rust-gix-date-0.8)
    (name "rust-gix-date")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-date" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00jrc86398553z2mdljx9vh8skqgdydhsrr11ak3148fcx2l25mw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs
       (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-gix-diff-0.37
  (package
    (name "rust-gix-diff")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m055q3sywj4i3c3xhdw75ir77l6pn3k9bhazimfvjdqkzv984wk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Calculate differences between various Git objects")
    (description "Calculate differences between various Git objects.  Part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-diff-0.36
  (package
    (inherit rust-gix-diff-0.37)
    (name "rust-gix-diff")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "134jv0rw7v9lgci65ynq4xy85mvy9rbvpg1n3zl0d0iq5haxp3bq"))))
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-imara-diff" ,rust-imara-diff-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-diff-0.30
  (package
    (inherit rust-gix-diff-0.36)
    (name "rust-gix-diff")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-diff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cqf3b2dypnd4hl1lwzj4sy8lfb6pdkbzczms95nla6chc0asach"))))
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.2)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-imara-diff" ,rust-imara-diff-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-discover-0.26
  (package
    (name "rust-gix-discover")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wlhqkrfyln97arr3hyllw4xc9gnk2qb4nkh70z8hy0i6bq5qpd4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-ref" ,rust-gix-ref-0.38)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-defer" ,rust-defer-0.1)
                                   ("rust-is-ci" ,rust-is-ci-1)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Discover Git repositories and check if a directory is a Git repository")
    (description
     "Discover Git repositories and check if a directory is a repository.
This package is part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-discover-0.25
  (package
    (inherit rust-gix-discover-0.26)
    (name "rust-gix-discover")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cyy5yahngnh16n08n9qjp69aavxa7nkrxzw0ajaj3jssx1pcl39"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-gix-hash" ,rust-gix-hash-0.13)
        ("rust-gix-path" ,rust-gix-path-0.10)
        ("rust-gix-ref" ,rust-gix-ref-0.37)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-defer" ,rust-defer-0.1)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-discover-0.19
  (package
    (inherit rust-gix-discover-0.25)
    (name "rust-gix-discover")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-discover" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ijg43psf0ijg0z7g0wwsh94c0058gg77pl1apkfzcpjs70cdadb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-ref" ,rust-gix-ref-0.30)
        ("rust-gix-sec" ,rust-gix-sec-0.8)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-defer" ,rust-defer-0.1)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-features-0.36
  (package
    (name "rust-gix-features")
    (version "0.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ckilzixrfylgnw5by3wpmym3ri0v9dbc60dkknfnnxvqsjs8ijd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate to integrate various capabilities using compile-time feature flags")
    (description
     "This package provides a crate to integrate various capabilities using
compile-time feature flags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-features-0.35
  (package
    (inherit rust-gix-features-0.36)
    (name "rust-gix-features")
    (version "0.35.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k98r3742xrlqwyaq13a9gazppm5swyx2h4hcmigg0s9mqiz97wv"))))
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-jwalk" ,rust-jwalk-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-prodash" ,rust-prodash-26)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-features-0.30
  (package
    (inherit rust-gix-features-0.35)
    (name "rust-gix-features")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-features" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0543ggy5vxr2lpi1405mcq93bshj3gfvpjgf13a60q5z14s4k31s"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-1)
        ("rust-bytesize" ,rust-bytesize-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-jwalk" ,rust-jwalk-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-prodash" ,rust-prodash-25)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha1-smol" ,rust-sha1-smol-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-bstr" ,rust-bstr-1))))))

(define-public rust-gix-filter-0.5
  (package
    (name "rust-gix-filter")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-filter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "024bv38s7q59wsp3whzpcra0x40mv53xii2jrnv5ni21rll0vr0v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; use of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-packetline-blocking" ,rust-gix-packetline-blocking-0.16)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this package implements Git filters in Rust")
    (description
     "This package provides a crate from the Gitoxide project implementing
Git filters in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.7
  (package
    (name "rust-gix-fs")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0db6bj773ssqvy03mi28glzy963cd1aaaxcbj4nv7s9glsmmz089"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-features" ,rust-gix-features-0.35))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "File system utilities for Gitoxide")
    (description
     "Gitoxide is a pure Rust implementation of Git.  This package provides
file system utilities for Gitoxide.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-fs-0.2
  (package
    (inherit rust-gix-fs-0.7)
    (name "rust-gix-fs")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-fs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ddah0c5ljpfpy2ab1gmgblli2lz4bpbgghm9vwqgnwa02bqknih"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-features" ,rust-gix-features-0.30))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-glob-0.14
  (package
    (name "rust-gix-glob")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06gz18spc8p4b3rbbbh2i2dz1ld2cw3ikgxkwmhjkspfqnc95cax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Gitoxide project crate dealing with pattern matching")
    (description
     "This package provides a crate of the gitoxide project dealing with pattern
matching.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-glob-0.13
  (package
    (inherit rust-gix-glob-0.14)
    (name "rust-gix-glob")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17dix59mc93m8z97ywkgpssjsmjgl4cfkifja7vxql8jy62nxmx9"))))
    (arguments
     `(#:tests? #f  ; no method named `trim_start` found for reference `&BStr`
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-glob-0.8
  (package
    (inherit rust-gix-glob-0.13)
    (name "rust-gix-glob")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-glob" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13jm1anf9xkp0hpzv9va72b9003kmwflx0ni0fbhf7xbh0gdw2nd"))))
    (arguments
     `(#:tests? #f  ; no method named `trim_start` found for reference `&BStr`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-hash-0.13
  (package
    (name "rust-gix-hash")
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q1xcp8f5prpyr4x62jixrlgm99snscnf87bny1faqvg4v1gi30z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Borrowed and owned git hash digests used to identify git objects")
    (description
     "Borrowed and owned git hash digests used to identify git objects.  This
package is part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hash-0.11
  (package
    (inherit rust-gix-hash-0.13)
    (name "rust-gix-hash")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bq986grpsfc6ddav5dlb8zvz1aky264dnnnmax2h1lsmpr2yhjb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-hashtable-0.4
  (package
    (name "rust-gix-hashtable")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05ka2z2z1qcxmfw8abvc5fgvygrrjfiaaz61h6701ba11146i4j0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Hashtable based data structures optimized to utilize ObjectId keys")
    (description
     "Hashtable based data structures optimized to utilize @code{ObjectId}
keys.  Part of Gitoxide a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-hashtable-0.2
  (package
    (inherit rust-gix-hashtable-0.4)
    (name "rust-gix-hashtable")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-hashtable" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13f5v6vghfpzxm5xkmk86gjhsjfqng9rpam37hqjssgkxkk4qprq"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-hash" ,rust-gix-hash-0.11)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12))))))

(define-public rust-gix-ignore-0.9
  (package
    (name "rust-gix-ignore")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ydq53isj75vf7gjggnv8yf2jimx7sfk5xpw66hvqi8nya6cq5d2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "This Gitoxide crate handles .gitignore files")
    (description
     "This crate is part of Gitoxide, it handles @code{.gitignore} files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ignore-0.8
  (package
    (inherit rust-gix-ignore-0.9)
    (name "rust-gix-ignore")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1qzmpylhwqqnnb7hcbwfbvblbzg3hzid4d2w42j2vc7nl51z8j5h"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-ignore-0.3
  (package
    (inherit rust-gix-ignore-0.8)
    (name "rust-gix-ignore")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ignore" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09anfy62zfsclkkvvrsp0bi99pny66hqn07pvc4fik0c3887yvzw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-glob" ,rust-gix-glob-0.8)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-unicode-bom" ,rust-unicode-bom-2))))))

(define-public rust-gix-index-0.26
  (package
    (name "rust-gix-index")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l0n7cld8m5fq1cnd3lyygmsirw5kzw7gxl8j082wbqv2b64yfn8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-11)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crate implements the Git index file")
    (description
     "Part of Gitoxide, a pure Rust implementation of Rust.  This package's
crate implements the Git index file.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-index-0.25
  (package
    (inherit rust-gix-index-0.26)
    (name "rust-gix-index")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dqp5ri3kh87qgy0gxzlr0m4jki8xk5sr8zm867hh4rws6ln6kgm"))))
    (arguments
     `(#:cargo-test-flags
       (list "--release" "--"
             "--skip=access::tests::entry_by_path_with_conflicting_file")
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-index-0.17
  (package
    (inherit rust-gix-index-0.25)
    (name "rust-gix-index")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-index" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10kvzsrn7wjgwb3y4xwj9szcg9j81mlkab04z9ii5cdzz9cajsv1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-gix-bitmap" ,rust-gix-bitmap-0.2)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-lock" ,rust-gix-lock-6)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-traverse" ,rust-gix-traverse-0.26)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-lock-11
  (package
    (name "rust-gix-lock")
    (version "11.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0drgl9qhkvlhjl0jc0lh2h7h3by1yg9wx4a8cqss8c4qlbk6ap3y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Git style lock files implemented in Rust")
    (description "Part of Gitoxide, a pure rust implementation of Git.  This
package provides git style lock files.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-lock-10
  (package
    (inherit rust-gix-lock-11)
    (name "rust-gix-lock")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15dazvw49bdx60366vngmrfn69rvxf0pr411a1ak6vbbigx9dz27"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-utils" ,rust-gix-utils-0.1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-lock-6
  (package
    (inherit rust-gix-lock-10)
    (name "rust-gix-lock")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-lock" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lpqi927lacn2vz22q2mhacc4plkrlz5whm779ax65kky3kdbi9y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gix-tempfile" ,rust-gix-tempfile-6)
        ("rust-gix-utils" ,rust-gix-utils-0.1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-macros-0.1
  (package
    (name "rust-gix-macros")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dhqxmj29k5b6l8y30ylkacscrzgz6ij18qr5l7mzmb8wrgcp2lx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs (("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Proc-macro utilities for gix")
    (description "Proc-macro utilities for Gitoxide.  Gitoxide is a pure Rust
implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-mailmap-0.20
  (package
    (name "rust-gix-mailmap")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01wzzs8gifl6i4vzwbx1ywzwgazy1db6yfh8b3bjsssy1pn5ycp2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Functions for parsing mailmap files in Gitoxide")
    (description
     "This package contains functions for parsing mailmap files.  It's part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-mailmap-0.19
  (package
    (inherit rust-gix-mailmap-0.20)
    (name "rust-gix-mailmap")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nx713bx8bi76h14zgg4786afpzryph16pcg43pndq19dslhzw20"))))
    (arguments
     `(#:tests? #f      ; undeclared crate `gix_testtools`
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-mailmap-0.13
  (package
    (inherit rust-gix-mailmap-0.19)
    (name "rust-gix-mailmap")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-mailmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0spslf6r4l51z640fxqpw2nq4j7imzz0jhxwy44y086948cp0ls6"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.21)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.9
  (package
    (name "rust-gix-negotiate")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zxnxfjjqxap8plkhz5f4h0gwm83ain229y2vhwwxjgcj7sdqp1a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; user of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements Git's negotiation algorithms as part of Gixoxide")
    (description
     "Gitoxide is a pure Rust implementation of Git.  This package consists of
Git's negotiation algorithms.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-negotiate-0.8
  (package
    (inherit rust-gix-negotiate-0.9)
    (name "rust-gix-negotiate")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01408hs82nhj40arkdx145cfmdccf7pydf89sywd3ihik6zrf5kg"))))
    (arguments
     `(#:tests? #f      ; user of undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-negotiate-0.2
  (package
    (inherit rust-gix-negotiate-0.8)
    (name "rust-gix-negotiate")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-negotiate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "137hyq036adi5kapw98vd9b0087lxljfkjaz81glmr0jx7qkwp4l"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-gix-commitgraph" ,rust-gix-commitgraph-0.16)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-revision" ,rust-gix-revision-0.15)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-object-0.38
  (package
    (name "rust-gix-object")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lbaz0mzsg5vvm5qvi1nf6f0hyz62hfx18xk3h57fn3z4r22l3vl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.28)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Immutable and mutable Git objects with decoding and encoding support")
    (description
     "Part of Gitoxide, a pure Rust Git implementation.  This package contains
functions to handle immutable and mutable git objects with decoding and encoding
support.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-object-0.37
  (package
    (inherit rust-gix-object-0.38)
    (name "rust-gix-object")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vzxayiss5k7pmlm0q4axaliidg9a3lq7bkv2ds775k7dihijzhy"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-object-0.30
  (package
    (inherit rust-gix-object-0.37)
    (name "rust-gix-object")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-object" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1is6hkzcv38m9k2yzja39h3d9s5rvs9vqpfb17kw7pj43kswh9l9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.21)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-validate" ,rust-gix-validate-0.7)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-itoa" ,rust-itoa-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-odb-0.54
  (package
    (name "rust-gix-odb")
    (version "0.54.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fql2p0xinkdaq7bybz12j1yw0b4lq3d1nl3sf2ad3qdp1nbac46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-pack" ,rust-gix-pack-0.44)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements various Git object databases")
    (description "Implements various Git object databases for Gitoxide.
Gitoxide is a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-odb-0.53
  (package
    (inherit rust-gix-odb-0.54)
    (name "rust-gix-odb")
    (version "0.53.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gv4zvmizqvxh4n3gnv3yzp83v3spklj0cf6rlrz38m3dcn3jsld"))))
    (arguments
     `(#:cargo-inputs (("rust-arc-swap" ,rust-arc-swap-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-pack" ,rust-gix-pack-0.43)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-odb-0.46
  (package
    (inherit rust-gix-odb-0.53)
    (name "rust-gix-odb")
    (version "0.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-odb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wzqj6r0vgr2v0v9578s1hikg9abbh85m2vwj0psrvkqca04s8sb"))))
    (arguments
     `(#:tests? #f      ; tests not included in release
       #:cargo-inputs
       (("rust-arc-swap" ,rust-arc-swap-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-pack" ,rust-gix-pack-0.36)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-serde" ,rust-serde-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-maplit" ,rust-maplit-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-gix-pack-0.44
  (package
    (name "rust-gix-pack")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hwphs7ks8pf6v4wrmhd4iy8vj1in95db4q6j82i9zyy60pblc8l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.37)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-11)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.34)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements Git packs and related data structures")
    (description
     "Git packs and related data structures for Gitoxide.  Gitoxide is a pure
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-pack-0.43
  (package
    (inherit rust-gix-pack-0.44)
    (name "rust-gix-pack")
    (version "0.43.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cqsxdqz6sdk1m96mpa0f33kddx1inx91gsbd72in7mk8lx20dkm"))))
    (arguments
     `(#:tests? #f      ; use of undeclared create gix_testtools
       #:cargo-inputs (("rust-clru" ,rust-clru-0.6)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-chunk" ,rust-gix-chunk-0.4)
                       ("rust-gix-diff" ,rust-gix-diff-0.36)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-pack-0.36
  (package
    (inherit rust-gix-pack-0.43)
    (name "rust-gix-pack")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "125gs2hw111izv81zcj9i4mm503ikchbfv3xn7npw0sn675i8akx"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-clru" ,rust-clru-0.6)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-chunk" ,rust-gix-chunk-0.4)
        ("rust-gix-diff" ,rust-gix-diff-0.30)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-tempfile" ,rust-gix-tempfile-6)
        ("rust-gix-traverse" ,rust-gix-traverse-0.26)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-serde" ,rust-serde-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-uluru" ,rust-uluru-3))))))

(define-public rust-gix-packetline-0.16
  (package
    (name "rust-gix-packetline")
    (version "0.16.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ayp00k4a6qgjjyrd2rmzxyx0n8h36dxscjn1pzil5b4x6qq90wa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-faster-hex" ,rust-faster-hex-0.8)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate of the gitoxide project implementing the pkt-line serialization format")
    (description
     "This package provides a crate of the gitoxide project implementing the
pkt-line serialization format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-packetline-blocking-0.16
  (package
    (name "rust-gix-packetline-blocking")
    (version "0.16.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-packetline-blocking" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f9dr79jz9y11qhf0syxwa4nvn4czpyka84hzshxd10wa3vrb0vx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-faster-hex" ,rust-faster-hex-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Duplicate of @code{gix-packetline} with @code{blocking-io} selected")
    (description
     "Part of Gitoxide, a pure Rust implementation of Git.  This package is a
duplicate of @code{gix-packetline} with the @code{blocking-io} feature pre-selected")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-path-0.10
  (package
    (name "rust-gix-path")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fx71d9p7iq4szgrijyw1nj82abg15ylyvaw7kh3l7hp2l0kf7ba"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Part of the Gitoxide project, this crate deals with paths and their conversions")
    (description
     "Part of the Gitoxide project, a lean and safe Rust implementation of
Git.  This crate deals with paths and their conversions")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-path-0.8
  (package
    (inherit rust-gix-path-0.10)
    (name "rust-gix-path")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-path" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0z5733b3z2wbnz1x0y2aq3gpanrhrlrqr4v4gjlqwl68ps69qq0q"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-trace" ,rust-gix-trace-0.1)
        ("rust-home" ,rust-home-0.5)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-pathspec-0.3
  (package
    (name "rust-gix-pathspec")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-pathspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zg5m586s0sxxwadlvsx9hrrkyym9d4q936kk0zyf6y58ydnrqn3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;undeclared crate gix_testtools
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-serial-test" ,rust-serial-test-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Capabilities to handle Git's pathspecs")
    (description
     "This package provides capabilities for handling Git's pathspecs.  It is
part of Gitoxide, a Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-prompt-0.7
  (package
    (name "rust-gix-prompt")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y26b3d7z222b223ir9qf8yqwhknzc3c5yksjffmwvsid4vr36jw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-config-value" ,rust-gix-config-value-0.14)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-expectrl" ,rust-expectrl-0.7)
                                   ("rust-serial-test" ,rust-serial-test-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Handles prompts in the terminal as part of Gitoxide")
    (description
     "Gitoxide is a Rust implementation of Git.  This crate handles the
terminals prompt.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-prompt-0.5
  (package
    (inherit rust-gix-prompt-0.7)
    (name "rust-gix-prompt")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-prompt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sm5b24jpcv4whzxymk6fpb1ph1hhq6842115fpcqqx0yk5dw8ic"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gix-command" ,rust-gix-command-0.2)
        ("rust-gix-config-value" ,rust-gix-config-value-0.12)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-rustix" ,rust-rustix-0.38)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-expectrl" ,rust-expectrl-0.7)
        ("rust-serial-test" ,rust-serial-test-2))))))

(define-public rust-gix-protocol-0.40
  (package
    (name "rust-gix-protocol")
    (version "0.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16qc5q53z5jdqb433106lzzifhf321zgv89ha65bxj8cq86p0yyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-btoi" ,rust-btoi-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.20)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-transport" ,rust-gix-transport-0.37)
                       ("rust-maybe-async" ,rust-maybe-async-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implementation of Git protocols that's part of Gitoxide")
    (description
     "This package implements Git protocols for Gitoxide.  Gitoxide is a pure
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-protocol-0.33
  (package
    (inherit rust-gix-protocol-0.40)
    (name "rust-gix-protocol")
    (version "0.33.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "093c9zlqxy8248nynyb909g12xkfxsv5z32j8w41yijwnic718cj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-gix-credentials" ,rust-gix-credentials-0.15)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-transport" ,rust-gix-transport-0.32)
        ("rust-maybe-async" ,rust-maybe-async-0.2)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16))))))

(define-public rust-gix-quote-0.4
  (package
    (name "rust-gix-quote")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01f9rm8m7pd6j6bhqiq1hgk11sn9pad27fsz8sj7n4nhgnlqcp27"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-btoi" ,rust-btoi-0.4)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Various quotations used by git")
    (description
     "This package provides a crate of the gitoxide project dealing with various
quotations used by git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.37
  (package
    (name "rust-gix-ref")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r4d0zpin2c62s4j88n32cd0gf1f2da1kp4yfr0kcq8bcr4vgri2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-actor" ,rust-gix-actor-0.27)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-lock" ,rust-gix-lock-10)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-tempfile" ,rust-gix-tempfile-10)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-memmap2" ,rust-memmap2-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-winnow" ,rust-winnow-0.5))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this crate handles Git references")
    (description "This package provides a crate to handle git references.
Part of Gitoxide, a project to create a pure Rust Git implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-ref-0.30
  (package
    (inherit rust-gix-ref-0.37)
    (name "rust-gix-ref")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-ref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "073y77045km55bb53ack6hzjd70mkj3rk6d8xxg8mkplas99kpgb"))))
    (arguments
     `(#:tests? #f      ; use of undeclared crate or module `gix_testtools`
       #:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-actor" ,rust-gix-actor-0.21)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-fs" ,rust-gix-fs-0.2)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-lock" ,rust-gix-lock-6)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-gix-tempfile" ,rust-gix-tempfile-6)
        ("rust-gix-validate" ,rust-gix-validate-0.7)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-nom" ,rust-nom-7)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-refspec-0.18
  (package
    (name "rust-gix-refspec")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07pniqh74kkm0n727m7wjxgrgwnaypljkhsh8nyw7wvh3rxwp588"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-revision" ,rust-gix-revision-0.22)
                       ("rust-gix-validate" ,rust-gix-validate-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Parsing and representing refspecs to Gitoxide")
    (description
     "This package parses and represents Git refspecs.  It's part of Gitoxide
a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-refspec-0.11
  (package
    (inherit rust-gix-refspec-0.18)
    (name "rust-gix-refspec")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-refspec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x0cayswa8m0yiybi8g3jimpc6jggfvrw6y53snxhvf8mciddgvj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-revision" ,rust-gix-revision-0.15)
        ("rust-gix-validate" ,rust-gix-validate-0.7)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.23
  (package
    (name "rust-gix-revision")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1215fz886j5gzf31kg32g566vm9pds5679d4d9vg79sr6k3pma9c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "This Gitoxide crate finds names for revisions and parsing specifications")
    (description
     "This package is part of Gitoxide, a pure Rust implementation of Git.  It
handles finding names and parsing specifications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revision-0.22
  (package
    (inherit rust-gix-revision-0.23)
    (name "rust-gix-revision")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version
                        ".tar.gz"))
       (sha256
        (base32
        "128fi6mblg4ic6h1q5vy2zq6vly8hxhi9vxkpkskaymby9fb3i68"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-gix-trace" ,rust-gix-trace-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revision-0.15
  (package
    (inherit rust-gix-revision-0.22)
    (name "rust-gix-revision")
    (version "0.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revision" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mrcjg8180n3kgbsngfaavzlfsqbmr905gjc0fdwx1x4sxngai2h"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-date" ,rust-gix-date-0.5)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-revwalk" ,rust-gix-revwalk-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.9
  (package
    (name "rust-gix-revwalk")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q7sgvkm0zdpp09v51jgv7c77zff82fvyr82dzc7dmjc5s4qqvd1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Utilities for walking the Git's revision graph")
    (description
     "Utilities for walking Git's revision graph.  This crate is part of
Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-revwalk-0.8
  (package
    (inherit rust-gix-revwalk-0.9)
    (name "rust-gix-revwalk")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yd27ip3xhxplsw6cmyjz1vk6q5c0qhkn33icx2hiwij21mhr1z9"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-revwalk-0.1
  (package
    (inherit rust-gix-revwalk-0.8)
    (name "rust-gix-revwalk")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-revwalk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jnkyb5yk8y76q5gbmdykm2snxn5v9dbc4jy3walz4a7hyx269mw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.16)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-sec-0.10
  (package
    (name "rust-gix-sec")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-sec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a0g1b25hkgc68svxl2rx22a64d16703yyyislpc1a15q0m59fcj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-windows" ,rust-windows-0.48))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Part of Gitoxide, this create provides a shared trust model")
    (description
     "This package is part of Gitoxide, it implements a shared trust model.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-sec-0.8
  (package
    (inherit rust-gix-sec-0.10)
    (name "rust-gix-sec")
    (version "0.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-sec" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iz9rcyx7lpb4gxg5gyv93ygp0n321c5xmrcjkmqm2annkbcn5cn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-2)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-windows" ,rust-windows-0.48))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-status-0.1
  (package
    (name "rust-gix-status")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-status" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yi2wl5c1zacr5nr8qj9g7rq9ylwp2ckvkjwgd0g41x75g202a08"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Provides @code{git status} functionality")
    (description
     "This package provides @code{git status} functionality to Gitoxide, a
Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-submodule-0.4
  (package
    (name "rust-gix-submodule")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-submodule" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dz2w3ikhbf938rfamlqkmxl9hznz4i7mm9dmgrd70lj5vl500fx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-config" ,rust-gix-config-0.30)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-pathspec" ,rust-gix-pathspec-0.3)
                       ("rust-gix-refspec" ,rust-gix-refspec-0.18)
                       ("rust-gix-url" ,rust-gix-url-0.24)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Functions for dealing with Git submodules")
    (description
     "Functions for dealing with Git submodules.  Part of Gitoxide a pure Rust
implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-tempfile-10
  (package
    (name "rust-gix-tempfile")
    (version "10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kdc21s0dnqnbzfdazpsw8fclnw1gi3w4np71qlmgp0i7s7rgq2s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Rust tempfile implementation with assured clean-up")
    (description
     "Part of Gitoxide a Rust implementation of Git.  This package provides a
tempfile capability with a global registry to assure clean-up.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-tempfile-6
  (package
    (inherit rust-gix-tempfile-10)
    (name "rust-gix-tempfile")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "047baclw78xkzjg04z5290x7vhcz270jpw7vdm25rp7922q5qy5k"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dashmap" ,rust-dashmap-5)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-fs" ,rust-gix-fs-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.12)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-signal-hook-registry" ,rust-signal-hook-registry-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gix-trace-0.1
  (package
    (name "rust-gix-trace")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-trace" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "129xhqmn7j07sg697g8bz4mas2xvv6hamh799y7vax84vrz15qg8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-document-features" ,rust-document-features-0.2)
        ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Crate to provide minimal `tracing` support")
    (description
     "This package provides a crate to provide minimal `tracing` support that
can be turned off to zero cost.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.38
  (package
    (name "rust-gix-transport")
    (version "0.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lp7bg7pj9l2na92bdrbx0zjybi7j88c26vm341z492f6s9rl81g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-async-trait" ,rust-async-trait-0.1)
                       ("rust-base64" ,rust-base64-0.21)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-curl" ,rust-curl-0.4)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gix-command" ,rust-gix-command-0.2)
                       ("rust-gix-credentials" ,rust-gix-credentials-0.21)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-packetline" ,rust-gix-packetline-0.16)
                       ("rust-gix-quote" ,rust-gix-quote-0.4)
                       ("rust-gix-sec" ,rust-gix-sec-0.10)
                       ("rust-gix-url" ,rust-gix-url-0.25)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-reqwest" ,rust-reqwest-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs (("rust-async-std" ,rust-async-std-1)
                                   ("rust-blocking" ,rust-blocking-1)
                                   ("rust-maybe-async" ,rust-maybe-async-0.2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Implements the Git transport layer for Gitoxide")
    (description
     "This package is part of Gitoxide a Rust implementation of Git.  It
provides an implementation of the Git transport layer.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-transport-0.37
  (package
    (inherit rust-gix-transport-0.38)
    (name "rust-gix-transport")
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kpv0aqx7c3hbsc14rk7c01yd7b73mm14d4swnn6hpi4d9p75v5r"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-gix-command" ,rust-gix-command-0.2)
        ("rust-gix-credentials" ,rust-gix-credentials-0.20)
        ("rust-gix-features" ,rust-gix-features-0.35)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-sec" ,rust-gix-sec-0.10)
        ("rust-gix-url" ,rust-gix-url-0.24)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-blocking" ,rust-blocking-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-transport-0.32
  (package
    (inherit rust-gix-transport-0.37)
    (name "rust-gix-transport")
    (version "0.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-transport" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05fzh609x4d6djizmrv5m98axinp2m5hcpk003bqw1x9v7z9z8v4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-lite" ,rust-futures-lite-1)
        ("rust-gix-command" ,rust-gix-command-0.2)
        ("rust-gix-credentials" ,rust-gix-credentials-0.15)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-packetline" ,rust-gix-packetline-0.16)
        ("rust-gix-quote" ,rust-gix-quote-0.4)
        ("rust-gix-sec" ,rust-gix-sec-0.8)
        ("rust-gix-url" ,rust-gix-url-0.19)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-blocking" ,rust-blocking-1)
        ("rust-maybe-async" ,rust-maybe-async-0.2))))))

(define-public rust-gix-traverse-0.34
  (package
    (name "rust-gix-traverse")
    (version "0.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12pk1w89kj978jdfsg2fwmq5p4gv0i0wydh6pxmbf6sfgpn51l0l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.22)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.9)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Ways to traverse Git commit graphs and trees for Gix")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package is used to traverse Git commit graphs and trees.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-traverse-0.33
  (package
    (inherit rust-gix-traverse-0.34)
    (name "rust-gix-traverse")
    (version "0.33.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "180vp1piwlalsv8qkmmif03l7h2kdxfx5vawkclbmb236smh9vr2"))))
    (arguments
     `(#:cargo-inputs (("rust-gix-commitgraph" ,rust-gix-commitgraph-0.21)
                       ("rust-gix-date" ,rust-gix-date-0.8)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-hashtable" ,rust-gix-hashtable-0.4)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-revwalk" ,rust-gix-revwalk-0.8)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-traverse-0.26
  (package
    (inherit rust-gix-traverse-0.33)
    (name "rust-gix-traverse")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-traverse" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n9kgzw5jal2jsqb56bpaj1xp35zp2hz6nf5klrjdgxl9jc2x15h"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-hashtable" ,rust-gix-hashtable-0.2)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-url-0.25
  (package
    (name "rust-gix-url")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01a0phpk3f0lrhavqm51cgpdwh925i2djiyslaj57ync24d7lhhc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-assert-matches" ,rust-assert-matches-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "This crate implements parsing and serialization of gix-url for Gitoxide")
    (description
     "This package is part of Gitoxide a Rust implementation of Git.  It
provides a crate for parsing and serialization of gix-url's.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-url-0.24
  (package
    (inherit rust-gix-url-0.25)
    (name "rust-gix-url")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03s2ds2z6whd44qapyvz4kqfpniik0issv6s09rbys4cdvsfq9b1"))))
    (arguments
     `(#:tests? #f ; undeclared crate gix_testtools
       #:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-home" ,rust-home-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-libtest-mimic" ,rust-libtest-mimic-0.6))))))

(define-public rust-gix-url-0.19
  (package
    (inherit rust-gix-url-0.24)
    (name "rust-gix-url")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-url" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qy8shn5s8r9fjchcqrz0xwg89lsjykd463naji4f864bbr3srpi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-home" ,rust-home-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-url" ,rust-url-2))))))

(define-public rust-gix-utils-0.1
  (package
    (name "rust-gix-utils")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-utils" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kz9sf2pwvinxv7dvarwzgqcw3v4653wrwfrlayfkdihvvi2aqny"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-fastrand" ,rust-fastrand-2))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Crate with `gitoxide` utilities that don't need feature toggles")
    (description
     "This package provides a crate with `gitoxide` utilities that don't need
feature toggles.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-validate-0.8
  (package
    (name "rust-gix-validate")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1idq44xvqjf2pxw9kxxjvi5cwklzc4wallp0arhqcnx40cmsnp70"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "Rust validation functions for various kinds of names in Git")
    (description "Part of Gitoxide, a pure Rust implementation of Git.  This
package contains validation functions for various kinds of names in Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-validate-0.7
  (package
    (inherit rust-gix-validate-0.8)
    (name "rust-gix-validate")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-validate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h4hr3rpgwc7ixyynjp53s9il3sb0gq8ad332k8drwyfn8vkg6xs"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-gix-worktree-0.27
  (package
    (name "rust-gix-worktree")
    (version "0.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zfpqbrxxwjjhjk1rn60rmajxm4f7ix2jbx44vklz9nv47kpkbyx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.20)
                       ("rust-gix-features" ,rust-gix-features-0.36)
                       ("rust-gix-fs" ,rust-gix-fs-0.8)
                       ("rust-gix-glob" ,rust-gix-glob-0.14)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.9)
                       ("rust-gix-index" ,rust-gix-index-0.26)
                       ("rust-gix-object" ,rust-gix-object-0.38)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Gitoxide functions for handling shared Git worktree types and utilities")
    (description
     "This package provides functions for handling shared Git worktree related
types and utilities.  Part of Gitoxide a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-0.26
  (package
    (inherit rust-gix-worktree-0.27)
    (name "rust-gix-worktree")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zhl1wlf4zl0ny1mwp7j24gzln63xy2grrh9cvaq5g8152bk4plz"))))
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-document-features" ,rust-document-features-0.2)
                       ("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-ignore" ,rust-gix-ignore-0.8)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-serde" ,rust-serde-1))))))

(define-public rust-gix-worktree-0.18
  (package
    (inherit rust-gix-worktree-0.26)
    (name "rust-gix-worktree")
    (version "0.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xjhxw6lmjhnmcnhajaks34dnsqgg7w7lf576hkl0m485sbav26k"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-1)
        ("rust-document-features" ,rust-document-features-0.2)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-gix-attributes" ,rust-gix-attributes-0.13)
        ("rust-gix-features" ,rust-gix-features-0.30)
        ("rust-gix-fs" ,rust-gix-fs-0.2)
        ("rust-gix-glob" ,rust-gix-glob-0.8)
        ("rust-gix-hash" ,rust-gix-hash-0.11)
        ("rust-gix-ignore" ,rust-gix-ignore-0.3)
        ("rust-gix-index" ,rust-gix-index-0.17)
        ("rust-gix-object" ,rust-gix-object-0.30)
        ("rust-gix-path" ,rust-gix-path-0.8)
        ("rust-io-close" ,rust-io-close-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-symlink" ,rust-symlink-0.1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-walkdir" ,rust-walkdir-2))))))

(define-public rust-gix-worktree-state-0.3
  (package
    (name "rust-gix-worktree-state")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bcpspxpvl8yybz8b0p64pjxb69b7kmkhsyv9jgarigjc1lv1bn3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bstr" ,rust-bstr-1)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-glob" ,rust-gix-glob-0.13)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-index" ,rust-gix-index-0.25)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-worktree" ,rust-gix-worktree-0.26)
                       ("rust-io-close" ,rust-io-close-0.3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis
     "Gitoxide project functions that set Git's worktree to a particular state")
    (description
     "This package provides functions for setting the Git worktree to a particular
state.  Used by Gitoxide a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gix-worktree-stream-0.5
  (package
    (name "rust-gix-worktree-stream")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gix-worktree-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sbmcar5r2wrv5dacx3381ykvc06fpfsfad4dfnw6p7428xfk7y8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ; using undeclared crates gix_worktree/gix_testtools
       #:cargo-inputs (("rust-gix-attributes" ,rust-gix-attributes-0.19)
                       ("rust-gix-features" ,rust-gix-features-0.35)
                       ("rust-gix-filter" ,rust-gix-filter-0.5)
                       ("rust-gix-fs" ,rust-gix-fs-0.7)
                       ("rust-gix-hash" ,rust-gix-hash-0.13)
                       ("rust-gix-object" ,rust-gix-object-0.37)
                       ("rust-gix-path" ,rust-gix-path-0.10)
                       ("rust-gix-traverse" ,rust-gix-traverse-0.33)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Byron/gitoxide")
    (synopsis "This crate generates a byte-stream from a git-tree")
    (description "This crate provides the ability to generate a byte-stream
from a git-tree.  It's part of Gitoxide, a pure Rust implementation of Git.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libgit2-sys-0.16
  (package
    (name "rust-libgit2-sys")
    (version "0.16.1+1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "030dnq7hz79qs4rxdllc3ailvqzc432jwwxk7g8av55hh0vbp8pj"))
       (modules '((guix build utils)))
       (snippet
        #~(begin (delete-file-recursively "libgit2")))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 openssl zlib))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Native bindings to the libgit2 library")
    (description
     "This package provides native Rust bindings to the @code{libgit2}
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libgit2-sys-0.15
  (package
    (inherit rust-libgit2-sys-0.16)
    (name "rust-libgit2-sys")
    (version "0.15.2+1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yllyq9wiryy257cfx8s7wadls24yzkxnhmbl95iz9ml3zhz43d8"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.3)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs (list libgit2-1.6 openssl zlib))))

(define-public rust-libgit2-sys-0.14
  (package
    (inherit rust-libgit2-sys-0.15)
    (name "rust-libgit2-sys")
    (version "0.14.2+1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1926x5f84ykr8j2lsdmb0n0bj4jz173j5bm722cgwx8hnpv9agbz"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs
     (list libgit2 openssl zlib))))

(define-public rust-libgit2-sys-0.13
  (package
    (inherit rust-libgit2-sys-0.14)
    (name "rust-libgit2-sys")
    (version "0.13.5+1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fkdgfqdkd38hfsyw3znq629gp7wdknzslym5l0g29k9q83fmrai"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs
     (list libgit2-1.4 openssl zlib))))

(define-public rust-libgit2-sys-0.12
  (package
    (inherit rust-libgit2-sys-0.14)
    (name "rust-libgit2-sys")
    (version "0.12.26+1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "153l8nvz9p8vyd5840xi6fwblvhpn3c33jwdwsznyq4f4jcwiq8r"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2")))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-libgit2-sys-0.10
  (package
    (inherit rust-libgit2-sys-0.12)
    (name "rust-libgit2-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0l9fvki7qxsl97vgzqwlv75nl213a5vxw7b1jaik97ala356pv6r"))
       (modules '((guix build utils)))
       (snippet
        '(begin (delete-file-recursively "libgit2") #t))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ;; Build dependencies:
        ("rust-cc" ,rust-cc-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-libgit2-sys-0.8
  (package
    (inherit rust-libgit2-sys-0.10)
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "libgit2") #t))))))

(define-public rust-libgit2-sys-0.6
  (package
    (inherit rust-libgit2-sys-0.10)
    (name "rust-libgit2-sys-6")
    (version "0.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libgit2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0myk79sxqj20bmj4ir3p81xnma9qnid5rrmlbkj5v68wgdpfdskf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-cmake" ,rust-cmake-0.1)
        ("rust-curl-sys" ,rust-curl-sys-0.4)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

;; Keep this package for future packaging of pijul.
(define-public rust-libpijul-0.12
  (package
    (name "rust-libpijul")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libpijul" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18d9n8xaq5ncq3375f0xrr96l8si1frczgzdlrz3fl1jby8vbl6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; TODO: Fix build
       #:tests? #f  ; backend::file_header::test_fileheader_alignment fails
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-bs58" ,rust-bs58-0.2)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-diffs" ,rust-diffs-0.3)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-hex" ,rust-hex-0.3)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-sanakirja" ,rust-sanakirja-0.10)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-toml" ,rust-toml-0.4))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list clang nettle openssl))
    (home-page "https://pijul.org/")
    (synopsis "Library component of the pijul version control system")
    (description
     "This crate contains the core API to access Pijul repositories.

The key object is a @code{Repository}, on which @code{Txn} (immutable
transactions) and @code{MutTxn} (mutable transactions) can be started, to
perform a variety of operations.

Another important object is a @code{Patch}, which encodes two different pieces
of information:

@itemize
@item Information about deleted and inserted lines between two versions of a
file.
@item Information about file moves, additions and deletions.
@end itemize")
    (license license:gpl2+)))
