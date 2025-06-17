;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2022-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Aaron Covrig <aaron.covrig.us@ieee.org>
;;; Copyright © 2024, 2025 Jordan Moore <lockbox@struct.foo>
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

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-module (gnu packages crates-check)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages pkg-config))

(define-public rust-criterion-0.5
  (package
    (name "rust-criterion")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anes" ,rust-anes-0.1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-cast" ,rust-cast-0.3)
        ("rust-ciborium" ,rust-ciborium-0.2)
        ("rust-clap" ,rust-clap-4)
        ("rust-criterion-plot" ,rust-criterion-plot-0.5)
        ("rust-csv" ,rust-csv-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-is-terminal" ,rust-is-terminal-0.4)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-oorandom" ,rust-oorandom-11)
        ("rust-plotters" ,rust-plotters-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smol" ,rust-smol-1)
        ("rust-tinytemplate" ,rust-tinytemplate-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis "Statistics-driven micro-benchmarking library")
    (description
     "This package provides a statistics-driven micro-benchmarking library.")
    ;; The user can choose either license.
    (license (list license:asl2.0 license:expat))))

(define-public rust-criterion-0.4
  (package
    (inherit rust-criterion-0.5)
    (name "rust-criterion")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "criterion" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jsl4r0yc3fpkyjbi8aa1jrm69apqq9rxwnjnd9brqmaq44nxiz7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-anes" ,rust-anes-0.1)
        ("rust-async-std" ,rust-async-std-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-cast" ,rust-cast-0.3)
        ("rust-ciborium" ,rust-ciborium-0.2)
        ("rust-clap" ,rust-clap-3)
        ("rust-criterion-plot" ,rust-criterion-plot-0.5)
        ("rust-csv" ,rust-csv-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-oorandom" ,rust-oorandom-11)
        ("rust-plotters" ,rust-plotters-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smol" ,rust-smol-1)
        ("rust-tinytemplate" ,rust-tinytemplate-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-criterion-0.3
  (package
    (inherit rust-criterion-0.4)
    (name "rust-criterion")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13yd64ah93gkbdv7qq4cr6rhgl9979jjcjk3gkhnav1b7glns7dh"))))
    (arguments
     `(#:cargo-test-flags '("--release" "--" "--skip=macros")
       #:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-cast" ,rust-cast-0.3)
        ("rust-clap" ,rust-clap-2)
        ("rust-criterion-plot" ,rust-criterion-plot-0.4)
        ("rust-csv" ,rust-csv-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-oorandom" ,rust-oorandom-11)
        ("rust-plotters" ,rust-plotters-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smol" ,rust-smol-1)
        ("rust-tinytemplate" ,rust-tinytemplate-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.5)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-criterion-0.2
  (package
    (inherit rust-criterion-0.3)
    (name "rust-criterion")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1543wlpc4p1kz7sqqa7ylr8bkdr8l4f34hy4bxj7krpkahwhaqq3"))))
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-criterion-plot" ,rust-criterion-plot-0.3)
        ("rust-csv" ,rust-csv-1)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand-core" ,rust-rand-core-0.3)
        ("rust-rand-os" ,rust-rand-os-0.1)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rayon-core" ,rust-rayon-core-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tinytemplate" ,rust-tinytemplate-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-criterion-cycles-per-byte-0.1
  (package
    (name "rust-criterion-cycles-per-byte")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion-cycles-per-byte" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15iw8zvyilx6k3a7z79vpzmpm6kkyds4c1ng3jlwfc43axd4hd4d"))))
    (build-system cargo-build-system)
    (arguments
     ;; error: criterion-cycles-per-byte currently relies on x86 or x86_64
     `(#:skip-build? ,(not (target-x86?))
       #:cargo-inputs
       (("rust-criterion" ,rust-criterion-0.3))))
    (home-page "https://crates.io/crates/criterion-cycles-per-byte")
    (synopsis "Measure time with CPU cycles for criterion")
    (description "This package lets you measure time with CPU cycles for
criterion.")
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-plot-0.5
  (package
    (name "rust-criterion-plot")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "criterion-plot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-itertool-num" ,rust-itertools-num-0.1)
        ("rust-num-complex" ,rust-num-complex-0.4)
        ("rust-rand" ,rust-rand-0.8))
       #:cargo-inputs
       (("rust-cast" ,rust-cast-0.3)
        ("rust-itertools" ,rust-itertools-0.10))))
    (home-page "https://github.com/bheisler/criterion.rs")
    (synopsis "Criterion's plotting library")
    (description "This package provides criterion's plotting library.")
    ;; The user can choose either license.
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-plot-0.4
  (package
    (inherit rust-criterion-plot-0.5)
    (name "rust-criterion-plot")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion-plot" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mys2zkizh5az6ax77m5aqifk0vz35rn0a6wykvmjx9gkzg9c2fh"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cast" ,rust-cast-0.2)
        ("rust-itertools" ,rust-itertools-0.10))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-criterion-plot-0.3
  (package
    (inherit rust-criterion-plot-0.4)
    (name "rust-criterion-plot")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion-plot" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13pv09z4ryp70qyzablkibwa2mh6c2852qq1sjr9wjigvwnj3ybn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-itertools" ,rust-itertools-0.8))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-mark-flaky-tests-1
  (package
    (name "rust-mark-flaky-tests")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mark-flaky-tests" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c29bflpb5aawl5vzcai2rhvphskvh7gdr5v9sq52lx0jmy4lv2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-mark-flaky-tests-macro" ,rust-mark-flaky-tests-macro-1))
       #:cargo-development-inputs (("rust-paste" ,rust-paste-1)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/GoldsteinE/mark-flaky-tests/")
    (synopsis "Mark, debug and auto-retry your flaky tests")
    (description
     "This package provides a way to mark, debug and auto-retry your flaky tests.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mark-flaky-tests-macro-1
  (package
    (name "rust-mark-flaky-tests-macro")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mark-flaky-tests-macro" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "115bb0pb4vb8pwm6lblcnc6zxxlk6w654njiphp696dj2vyiz2q7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/GoldsteinE/mark-flaky-tests/")
    (synopsis "Mark, debug and auto-retry your flaky tests (proc-macro crate)")
    (description
     "This package provides a way to mark, debug and auto-retry your flaky tests
(proc-macro crate).")
    (license (list license:expat license:asl2.0))))

(define-public rust-mock-instant-0.3
  (package
    (name "rust-mock-instant")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mock_instant" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "180yr3i44a98w1mj36dd8xmym33rbzndpj0j1g13di52n8g8crlk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/museun/mock_instant")
    (synopsis "Mock an std::time::Instant")
    (description
     "This package provides a simple way to mock an std::time::Instant in Rust.")
    (license license:bsd-0)))

(define-public rust-mock-instant-0.2
  (package
    (inherit rust-mock-instant-0.3)
    (name "rust-mock-instant")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mock_instant" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0vg0kmz96zazjdq57l57nm24mc2in57y090ywcq827xq8fi2jzki"))))
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1))))))

(define-public rust-mockall-0.13
  (package
    (name "rust-mockall")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockall" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lir70dd9cnsjlf20gi3i51ha9n7mlrkx74bx5gfszlcdk6bz9ir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-downcast" ,rust-downcast-0.11)
                       ("rust-fragile" ,rust-fragile-2)
                       ("rust-mockall-derive" ,rust-mockall-derive-0.13)
                       ("rust-predicates" ,rust-predicates-3)
                       ("rust-predicates-tree" ,rust-predicates-tree-1))
       #:cargo-development-inputs (("rust-async-trait" ,rust-async-trait-0.1)
                                   ("rust-auto-enums" ,rust-auto-enums-0.8)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-mockall-double" ,rust-mockall-double-0.3)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "Powerful mock object library for Rust")
    (description
     "This package provides a powerful mock object library for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mockall-0.11
  (package
    (inherit rust-mockall-0.13)
    (name "rust-mockall")
    (version "0.11.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mockall" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15kww0a3wv300wkksc6zj0kz1jwk0hyly48daxs2vvpj300lk12c"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-downcast" ,rust-downcast-0.11)
        ("rust-fragile" ,rust-fragile-2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-mockall-derive" ,rust-mockall-derive-0.11)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-predicates-tree" ,rust-predicates-tree-1))
       #:cargo-development-inputs
       (("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-mockall-double" ,rust-mockall-double-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tracing" ,rust-tracing-0.1))))))

(define-public rust-mockall-derive-0.13
  (package
    (name "rust-mockall-derive")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mockall_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1608qajqrz23xbvv81alc6wm4l24as1bsqg4shdh3sggq8231ji5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "Procedural macros for Mockall")
    (description "This package provides procedural macros for Mockall.")
    (license (list license:expat license:asl2.0))))

(define-public rust-mockall-derive-0.11
  (package
    (inherit rust-mockall-derive-0.13)
    (name "rust-mockall-derive")
    (version "0.11.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mockall_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fvc9kwjcc9ia6ng7z9z02b4qkl9dvsx9m4z51xz9i0mj1k7bki2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1))))))

(define-public rust-mockall-double-0.3
  (package
    (name "rust-mockall-double")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mockall_double" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s0k85929bf8afvdgq8m2vs8haqpkg9ysdimw7inl99mmkjrdjpi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/asomers/mockall")
    (synopsis "Double test adapter that works well with Mockall")
    (description
     "This crate makes it even easier to use mocking by providing a way to
select the mock struct at compile time.  Used with the Mockall crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nextest-filtering-0.12
  (package
    (name "rust-nextest-filtering")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nextest-filtering" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fz7w2qsmh98c246x16l9j5xypsj1nbc715wszajjkjiv7hbibvy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--" "--exact"
                            ;; artifacts not packaged
                            "--skip=test_binary_query"
                            "--skip=test_expr_binary"
                            "--skip=test_expr_binary_id_glob"
                            "--skip=test_expr_deps"
                            "--skip=test_expr_kind"
                            "--skip=test_expr_kind_partial"
                            "--skip=test_expr_package_contains"
                            "--skip=test_expr_package_equal"
                            "--skip=test_expr_package_regex"
                            "--skip=test_expr_platform"
                            "--skip=test_expr_rdeps"
                            "--skip=test_expr_test"
                            "--skip=test_expr_test_difference::with_and_not"
                            "--skip=test_expr_test_difference::with_minus"
                            "--skip=test_expr_test_intersect::with_ampersand"
                            "--skip=test_expr_test_intersect::with_and"
                            "--skip=test_expr_test_not"
                            "--skip=test_expr_test_union::with_or"
                            "--skip=test_expr_test_union::with_pipe"
                            "--skip=test_expr_test_union::with_plus"
                            "--skip=test_expr_with_no_matching_packages")
       #:cargo-inputs
       (("rust-globset" ,rust-globset-0.4)
        ("rust-guppy" ,rust-guppy-0.17)
        ("rust-miette" ,rust-miette-7)
        ("rust-nextest-metadata" ,rust-nextest-metadata-0.12)
        ("rust-nextest-workspace-hack" ,rust-nextest-workspace-hack-0.1)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-recursion" ,rust-recursion-0.5)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.8)
        ("rust-test-strategy" ,rust-test-strategy-0.4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-winnow" ,rust-winnow-0.6)
        ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))
       #:cargo-development-inputs (("rust-test-case" ,rust-test-case-3))))
    (home-page "https://github.com/nextest-rs/nextest")
    (synopsis "Filtering DSL for cargo-nextest")
    (description
     "This package provides a filtering DSL for cargo-nextest.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nextest-metadata-0.12
  (package
    (name "rust-nextest-metadata")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nextest-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cx6cdzbgmf5imjcb6vpm6xpdx5nrl3w079m0vm7la0chj21yn7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags '("--lib") ; doctests fail
       #:cargo-inputs
       (("rust-camino" ,rust-camino-1)
        ("rust-nextest-workspace-hack" ,rust-nextest-workspace-hack-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smol-str" ,rust-smol-str-0.3)
        ("rust-target-spec" ,rust-target-spec-3))
       #:cargo-development-inputs (("rust-test-case" ,rust-test-case-3))))
    (home-page "https://github.com/nextest-rs/nextest")
    (synopsis "Structured access to nextest machine-readable output")
    (description
     "This package provides structured access to nextest
machine-readable output.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nextest-runner-0.70
  (package
    (name "rust-nextest-runner")
    (version "0.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nextest-runner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rg6hf8jbn82rv61m9f0yjjmz7bra8mgsilmidiq085sjsi7ymq4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-1)
        ("rust-async-scoped" ,rust-async-scoped-0.9)
        ("rust-atomicwrites" ,rust-atomicwrites-0.4)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-camino" ,rust-camino-1)
        ("rust-camino-tempfile" ,rust-camino-tempfile-1)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.19)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-config" ,rust-config-0.14)
        ("rust-console-subscriber" ,rust-console-subscriber-0.4)
        ("rust-crossterm" ,rust-crossterm-0.28)
        ("rust-debug-ignore" ,rust-debug-ignore-1)
        ("rust-derive-where" ,rust-derive-where-1)
        ("rust-duct" ,rust-duct-0.13)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-future-queue" ,rust-future-queue-0.3)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-guppy" ,rust-guppy-0.17)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-home" ,rust-home-0.5)
        ("rust-http" ,rust-http-1)
        ("rust-humantime-serde" ,rust-humantime-serde-1)
        ("rust-indent-write" ,rust-indent-write-2)
        ("rust-indexmap" ,rust-indexmap-2)
        ("rust-indicatif" ,rust-indicatif-0.17)
        ("rust-is-ci" ,rust-is-ci-1)
        ("rust-itertools" ,rust-itertools-0.13)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-miette" ,rust-miette-7)
        ("rust-mukti-metadata" ,rust-mukti-metadata-0.3)
        ("rust-newtype-uuid" ,rust-newtype-uuid-1)
        ("rust-nextest-filtering" ,rust-nextest-filtering-0.12)
        ("rust-nextest-metadata" ,rust-nextest-metadata-0.12)
        ("rust-nextest-workspace-hack" ,rust-nextest-workspace-hack-0.1)
        ("rust-nix" ,rust-nix-0.29)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-owo-colors" ,rust-owo-colors-4)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
        ("rust-quick-junit" ,rust-quick-junit-0.5)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-self-update" ,rust-self-update-0.41)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-ignored" ,rust-serde-ignored-0.1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-smol-str" ,rust-smol-str-0.3)
        ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
        ("rust-supports-unicode" ,rust-supports-unicode-3)
        ("rust-swrite" ,rust-swrite-0.1)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-target-spec" ,rust-target-spec-3)
        ("rust-target-spec-miette" ,rust-target-spec-miette-0.4)
        ("rust-thiserror" ,rust-thiserror-2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-toml-edit" ,rust-toml-edit-0.22)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-unicode-ident" ,rust-unicode-ident-1)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
        ("rust-win32job" ,rust-win32job-2)
        ("rust-windows-sys" ,rust-windows-sys-0.59)
        ("rust-xxhash-rust" ,rust-xxhash-rust-0.8)
        ("rust-zstd" ,rust-zstd-0.13))
       #:cargo-development-inputs
       (("rust-color-eyre" ,rust-color-eyre-0.6)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-insta" ,rust-insta-1)
        ("rust-maplit" ,rust-maplit-1)
        ("rust-pathdiff" ,rust-pathdiff-0.2)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-test-case" ,rust-test-case-3)
        ("rust-test-strategy" ,rust-test-strategy-0.4))))
    (native-inputs (list pkg-config))
    (inputs (list (list zstd "lib")))
    (home-page "https://github.com/nextest-rs/nextest")
    (synopsis "Core runner logic for cargo nextest")
    (description
     "This package provides core runner logic for cargo nextest.")
    (license (list license:expat license:asl2.0))))

(define-public rust-nextest-workspace-hack-0.1
  (package
    (name "rust-nextest-workspace-hack")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nextest-workspace-hack" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cxjiwja0idhd8as3drl2wgk5y7f84k2rrk67pbxk7kkk1m881nr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nextest-rs/nextest")
    (synopsis "workspace-hack package")
    (description
     "This package provides a workspace-hack package, managed by hakari.")
    (license license:cc0)))

(define-public rust-quickcheck-1
  (package
    (name "rust-quickcheck")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mjhkfqwrb8mdyxdqr4zzbj1rm5dfx25n9zcc25lb6fxwiw673sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/BurntSushi/quickcheck")
    (synopsis "Automatic property based testing with shrinking")
    (description
     "QuickCheck is a way to do property based testing using randomly generated
input.  This crate comes with the ability to randomly generate and shrink
integers, floats, tuples, booleans, lists, strings, options and results.")
    (license (list license:unlicense license:expat))))

(define-public rust-quickcheck-0.9
  (package
    (inherit rust-quickcheck-1)
    (name "rust-quickcheck")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pwl7j21wmf843kpa9gr0byb40hg975ghjrwp0yxcym99bkq6j54"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-core" ,rust-rand-core-0.5))))))

(define-public rust-quickcheck-0.8
  (package
    (inherit rust-quickcheck-0.9)
    (name "rust-quickcheck")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mkl4wnvvjk4m32aq3an4ayfyvnmbxnzcybfm7n3fbsndb1xjdcw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rand-core" ,rust-rand-core-0.4))))))

(define-public rust-quickcheck-0.7
  (package
    (inherit rust-quickcheck-0.9)
    (name "rust-quickcheck")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "05pqzja6fwdyrs1za5vmxb9ifb993knmpdsrs1fs2wyz9qz7slyl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rand-core" ,rust-rand-core-0.2))))))

(define-public rust-quickcheck-0.6
  (package
    (inherit rust-quickcheck-0.9)
    (name "rust-quickcheck")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dyazm2fcq0v9fscq1a7597zsvdl9f0j8c2bfj1jm2nlzz2sn6y0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-quickcheck-0.5
  (package
    (inherit rust-quickcheck-0.9)
    (name "rust-quickcheck")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jzm1ygfbn4igaq14b9nipc8yvsn6c8panpgd1qiy5r2insjllyd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.4)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-quickcheck-0.4
  (package
    (inherit rust-quickcheck-0.5)
    (name "rust-quickcheck")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01hligcv1h4pvc8ykch65qjzi7jgcq2s462v69j27slc84fl3hh2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-quickcheck-0.2
  (package
    (inherit rust-quickcheck-0.4)
    (name "rust-quickcheck")
    (version "0.2.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vb4acppaavlnchzc1jmn5wlkgir9x9gmhgp97bavyxxqxgsg1nh"))))))

(define-public rust-quickcheck-macros-1
  (package
    (name "rust-quickcheck-macros")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck_macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s8nh0fmmzq3fd7928qcp2syvymlyv1pmww6fxcaj5np48r6jamj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/BurntSushi/quickcheck")
    (synopsis "Macro attribute for quickcheck")
    (description
     "This package provides a macro attribute for quickcheck.")
    (license (list license:unlicense license:expat))))

(define-public rust-quickcheck-macros-0.9
  (package
    (inherit rust-quickcheck-macros-1)
    (name "rust-quickcheck-macros")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck_macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zsb9b4jpg7qvbiym4v8y9pgqk7p1g4f5hn9gp0fnzz9v1pib330"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.9))))))

(define-public rust-quickcheck-macros-0.8
  (package
    (inherit rust-quickcheck-macros-0.9)
    (name "rust-quickcheck-macros")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck_macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b3mhn0xcrdd3fkbkx3rghhivwzwil8w991ngp6gaj70l72c3pyp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8))))))

(define-public rust-tango-bench-0.6
  (package
    (name "rust-tango-bench")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tango-bench" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gj2jgfdmwhrdggqh3yp8h33n1jrz6f3drmzg3nny83gihsj4y15"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alloca" ,rust-alloca-0.4)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-colorz" ,rust-colorz-1)
                       ("rust-glob-match" ,rust-glob-match-0.2)
                       ("rust-goblin" ,rust-goblin-0.7)
                       ("rust-libloading" ,rust-libloading-0.8)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-scroll" ,rust-scroll-0.11)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/bazhenov/tango")
    (synopsis "Tango benchmarking harness")
    (description "This package provides tango benchmarking harness.")
    (license license:expat)))

(define-public rust-tiny-bench-0.3
  (package
    (name "rust-tiny-bench")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiny-bench" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j2rsyspqbk89y6zjndpc6d36ljir1ymyj15vv6jxhaphl6q9nng"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/EmbarkStudios/tiny-bench")
    (synopsis "tiny benchmarking library")
    (description "This package provides a tiny benchmarking library.")
    (license (list license:expat license:asl2.0))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
