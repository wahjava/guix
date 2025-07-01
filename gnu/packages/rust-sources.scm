;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023-2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages rust-sources)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages textutils))

;;;
;;; Commit changes to: https://codeberg.org/guix/guix-rust-registry
;;;

;;;
;;; Cargo workspaces and Rust libraries requiring external inputs to unbundle.
;;; These packages are hidden, as they are not interesting to users.
;;;

(define gemoji-source-for-rust-deunicode-1
  (let ((version "4.1.0"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/github/gemoji")
            (commit (string-append "v" version))))
      (file-name "gemoji-checkout")
      (sha256
       (base32
        "1yhs9lj9gnzbvimv0y5f1a4my0slbvygkcjjkaxd4wkkyfvfbkxy")))))

(define-public rust-alacritty-0.25.1-dev.828457c
 (hidden-package
  (package
    (name "rust-alacritty")
    (version "0.25.1-dev")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/zed-industries/alacritty.git")
         (commit "828457c9ff1f7ea0a0469337cc8a37ee3a1b0590")))
       (file-name
        (git-file-name "rust-alacritty" "0.25.1-dev.828457c"))
       (sha256
        (base32 "1rw6d14j41yvkpk58ngv626bwrwdm3vzyc4gh5lg7fi8x0fda5v0"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:install-source? #t
           #:cargo-package-crates ''("alacritty_config_derive" "alacritty_config" "alacritty_terminal" "alacritty")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-cargo-manifests
                 (lambda _
                   ;; Break the circular dev-dependency for packaging.
                   ;; We need that because we aren't using "cargo package --workspace".
                   (substitute* "alacritty_config_derive/Cargo.toml"
                     (("dev-dependencies[.]alacritty_config")
                      "dummy.dev-dependencies.alacritty_config")))))))
    (inputs (cargo-inputs 'rust-alacritty-0.25.1-dev.828457c))
    (home-page "https://github.com/zed-industries/alacritty")
    (synopsis "Terminal emulator library")
    (description
     "This package provides a terminal emulator library.")
    (license license:asl2.0))))

(define-public rust-async-stripe-0.40.0.3672dd4
  (hidden-package
   (package
     (name "rust-async-stripe")
     (version "0.40.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/async-stripe")
          (commit "3672dd4efb7181aa597bf580bf5a2f5d23db6735")))
        (file-name
         (git-file-name "rust-async-stripe" "0.40.0.3672dd4"))
        (sha256
         (base32 "1k4015jzah505bkdjpdb6i9a5x5n6l91n3mp9ajkiw2cd455hmwi"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("async-stripe"
                                        ;"stripe-openapi-codegen" ; requires old clap
                                      )
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'patch
                  (lambda _
                    (substitute* "Cargo.toml"
                      ;; This optional dependency has a large number of outdated dependencies.
                      (("^rocket = .*$")
                       "\n")
                      ;; This optional dependency has a number of outdated dependencies.
                      (("^surf = .*$")
                       "\n")
                      ;; This optional dependency has a number of outdated dependencies.
                      (("^axum = .*$")
                       "\n")
                      ;; This optional dependency has a number of outdated dependencies.
                      (("^httpmock = .*$")
                       "\n")
                      ;; Remove reference to it.
                      (("\"surf\",")
                       "")))))))
     (inputs (cargo-inputs 'rust-async-stripe-0.40.0.3672dd4))
     (home-page "https://github.com/microsoft/python-environment-tools")
     (synopsis "Python environment tools")
     (description
      "This package provides Python environment tools.")
     (license license:asl2.0))))

(define-public rust-blade-0.6.0.e0ec4e7
  (hidden-package
   (package
     (name "rust-blade")
     (version "0.6.0.e0ec4e7")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/kvark/blade")
          (commit "e0ec4e720957edd51b945b64dd85605ea54bcfe5")))
        (file-name
         (git-file-name "rust-blade" version))
        (sha256
         (base32 "1ncazvjmla1pqkjsbjg6idj6v5a5xz74k09xkdmf48m29yi480ig"))
        (modules '((guix build utils)))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("blade-asset" 
                                      "blade-graphics" 
                                      "blade-macros"
                                      "blade-util"
                                        ; we don't have egui packaged "blade-egui"
                                        ; bevy "blade-render"
                                      ;; render "blade-helpers"
                                      )))
     (inputs (cargo-inputs 'rust-blade-0.6.0.e0ec4e7))
     (home-page "https://github.com/kvark/blade")
     (synopsis "Sharp and simple graphics library")
     (description
      "This package provides @code{blade}, the sharp and simple graphics
library.")
     (license license:expat))))

(define-public rust-deunicode-1
  (hidden-package
   (package
     (name "rust-deunicode")
     (version "1.6.1")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/kornelski/deunicode")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0bhbkfhh5qd4ygx47jj7j6h722i1skmkwashmj5wlh648f5gdmx4"))
               (modules '((guix build utils)))
               (snippet
                '(for-each delete-file-recursively
                           '("scripts/gemoji"
                             "src/mapping.txt"
                             "src/pointers.bin")))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates ''("deunicode")
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'compress-data
                  (lambda _
                    (with-directory-excursion "scripts"
                      (copy-recursively
                       #+(this-package-native-input "gemoji-checkout")
                       "gemoji")
                      (invoke "cargo" "run")))))))
     (native-inputs (list gemoji-source-for-rust-deunicode-1))
     ;; scripts/Cargo.lock
     (inputs (cargo-inputs 'rust-deunicode-1))
     (home-page "https://lib.rs/crates/deunicode")
     (synopsis "Convert Unicode strings to pure ASCII")
     (description
      "This package converts Unicode strings to pure ASCII by intelligently
transliterating them.  It supports Emoji and Chinese.")
     (license license:bsd-3))))

(define-public rust-hypher-0.1
  (hidden-package
   (package
     (name "rust-hypher")
     (version "0.1.5")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/typst/hypher")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1r01hhgxp5fmz1bgy11ilajd67lgfh7kqvd258c58c6y3w3rxpjq"))
        (modules '((guix build utils)))
        ;; Pre-generated.
        (snippet '(for-each delete-file (find-files "tries")))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates ''("hypher")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'regenerate
             (lambda _
               ;; Stop the attempted dependency retrieval for a bench-marking
               ;; tool.
               (substitute* "Cargo.toml" (("^members.+$") ""))
               ;; --no-default-features lets us avoid using the upstream
               ;; source's pre-generated tries when running the
               ;; trie-regenerating "generate" test. We are throwing library
               ;; binary away, anyways, because we only want the source.
               ;; See also: <https://github.com/typst/hypher/issues/19>.
               (false-if-exception
                (invoke "cargo" "test" "--test" "generate"
                        "--no-default-features"))
               (delete-file-recursively "target"))))))
     (home-page "https://github.com/typst/hypher")
     (synopsis "Separate words into syllables")
     (description "@code{hypher} is a Rust library for syllabification.")
     (license (list license:expat license:asl2.0)))))

(define-public rust-pcre2-utf32-0.2
  (hidden-package
   (package
     (name "rust-pcre2-utf32")
     (version "0.2.9")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/fish-shell/rust-pcre2")
                     (commit (string-append version "-utf32"))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0mhjw7fvrzxb3fd0c534a17qgy6svz0z8269d2fs6q8aw11610mr"))
               (modules '((guix build utils)))
               (snippet '(delete-file-recursively "pcre2-sys/upstream"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates
            ''("pcre2-sys" "pcre2")))
     (inputs (cargo-inputs 'rust-pcre2-utf32-0.2))
     (home-page "https://github.com/fish-shell/rust-pcre2")
     (synopsis "High level wrapper library for PCRE2")
     (description
      "This package provides @code{fish} shell's fork of @code{rust-pcre2} with
UTF-32 support.")
     (license (list license:expat license:unlicense)))))

(define-public rust-pipewire-for-niri
  (let ((commit "fd3d8f7861a29c2eeaa4c393402e013578bb36d9")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-pipewire")
       (version (git-version "0.8.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://gitlab.freedesktop.org/pipewire/pipewire-rs.git")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1hzyhz7xg0mz8a5y9j6yil513p1m610q3j9pzf6q55vdh5mcn79v"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates
              ''("libspa-sys" "libspa" "pipewire-sys" "pipewire")
              #:phases
              #~(modify-phases %standard-phases
                  ;; Avoid circular dependency.
                  (add-after 'unpack 'remove-dev-dependencies
                    (lambda _
                      (substitute* "libspa/Cargo.toml"
                        (("^pipewire.*") "")))))))
       (inputs (cargo-inputs 'rust-pipewire-for-niri))
       (home-page "https://pipewire.org/")
       (synopsis "Rust bindings for PipeWire")
       (description "This package provides Rust bindings for PipeWire.")
       (license license:expat)))))

(define-public rust-pubgrub-for-uv
  (let ((commit "b70cf707aa43f21b32f3a61b8a0889b15032d5c4")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-pubgrub")
       (version (git-version "0.3.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/astral-sh/pubgrub")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "08rfk4hh2cx4v8fi62j365mwga3fgww9wcfszq7i5g4zmlhp8p8l"))
                 (modules '((guix build utils)))
                 ;; Pretend to be version 0.3.0.
                 (snippet
                  '(substitute* "Cargo.toml"
                     (("0\\.3\\.0-alpha\\.1") "0.3.0")))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("version-ranges" "pubgrub")))
       (inputs (cargo-inputs 'rust-pubgrub-for-uv))
       (home-page "https://github.com/pubgrub-rs/pubgrub")
       (synopsis "PubGrub version solving algorithm")
       (description
        "This package provides the @code{PubGrub} version solving algorithm.")
       (license license:mpl2.0)))))

(define-public rust-ring-0.17
  (hidden-package
   (package
     (name "rust-ring")
     (version "0.17.8")                 ;Not tagged.
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/briansmith/ring")
                     (commit "fa98b490bcbc99a01ff150896ec74c1813242d7f")))
               (file-name (git-file-name "rust-ring" version))
               (sha256
                (base32 "0rqfal81bf4l3dja98cajfjq2jbz1rcx7xdp2r33cxrm5y5psr28"))
               (patches (search-patches "rust-ring-0.17-ring-core.patch"))
               (modules '((guix build utils)))
               (snippet
                #~(begin
                    ;; It turns out Guix's nasm works just fine here.
                    (substitute* "build.rs"
                      (("./target/tools/windows/nasm/nasm") "nasm"))
                    ;; These files are pregenerated:
                    (delete-file "crypto/curve25519/curve25519_tables.h")
                    (delete-file "crypto/fipsmodule/ec/p256-nistz-table.h")
                    (delete-file "crypto/fipsmodule/ec/p256_table.h")
                    ;; As seen in git between 0.17.0 and 0.17.1.
                    (substitute* "crypto/curve25519/make_curve25519_tables.py"
                      (("static const uint8_t k25519Precomp")
                       "const uint8_t k25519Precomp"))
                    ;; This file causes problems during the 'package phase and
                    ;; is not distributed with the packaged crate.
                    (delete-file-recursively "bench")
                    (substitute* "Cargo.toml"
                      (("\"bench\",") ""))))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:skip-build? #t
       #:cargo-package-crates ''("ring")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'regenerate
             (lambda _
               (setenv "HOME" (getcwd))
               (with-directory-excursion "crypto/curve25519"
                 (with-output-to-file "curve25519_tables.h"
                   (lambda _ (invoke "python3" "make_curve25519_tables.py"))))
               (with-directory-excursion "crypto/fipsmodule/ec"
                 (invoke "go" "run" "make_tables.go")
                 (invoke "go" "run" "make_ec_scalar_base_mult_tests.go"))
               (format #t "Generating the pregenerated files ...~%")
               (force-output)
               (mkdir-p "pregenerated/tmp/ring_core_generated")

               ;; We generate all the files which upstream would normally be
               ;; generate by using 'RING_PREGENERATE_ASM=1 cargo build
               ;; --target-dir=target/pregenerate_asm' in order to not include
               ;; a dependency on cargo when generating the sources.
               (define (prefix script)
                 (string-append
                  "pregenerated/"
                  (string-drop-right
                   (string-drop script
                                (string-index-right script #\/)) 3)))

               (for-each
                (lambda (script)
                  (invoke "perl" script "ios64"
                          (string-append (prefix script) "-ios64.S"))
                  (invoke "perl" script "linux64"
                          (string-append (prefix script) "-linux64.S"))
                  (invoke "perl" script "win64"
                          (string-append (prefix script) "-win64.S")))
                '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                  "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                  "crypto/chacha/asm/chacha-armv8.pl"
                  "crypto/cipher_extra/asm/chacha20_poly1305_armv8.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-armv8.pl"
                  "crypto/fipsmodule/bn/asm/armv8-mont.pl"
                  "crypto/fipsmodule/ec/asm/p256-armv8-asm.pl"
                  "crypto/fipsmodule/modes/asm/ghash-neon-armv8.pl"
                  "crypto/fipsmodule/modes/asm/aesv8-gcm-armv8.pl"
                  "crypto/fipsmodule/sha/asm/sha512-armv8.pl"))

               (for-each
                (lambda (arch)
                  (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-armv8.pl"
                          arch (string-append
                                "pregenerated/sha256-armv8-" arch ".S")))
                '("ios64" "linux64" "win64"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "linux32"
                          (string-append (prefix script) "-linux32.S")))
                '("crypto/fipsmodule/aes/asm/aesv8-armx.pl"
                  "crypto/fipsmodule/modes/asm/ghashv8-armx.pl"
                  "crypto/fipsmodule/aes/asm/bsaes-armv7.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-armv7.pl"
                  "crypto/fipsmodule/bn/asm/armv4-mont.pl"
                  "crypto/chacha/asm/chacha-armv4.pl"
                  "crypto/fipsmodule/modes/asm/ghash-armv4.pl"
                  "crypto/fipsmodule/sha/asm/sha256-armv4.pl"
                  "crypto/fipsmodule/sha/asm/sha512-armv4.pl"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "elf"
                          "-fPIC" "-DOPENSSL_IA32_SSE2"
                          (string-append (prefix script) "-elf.S"))
                  (invoke "perl" script "win32n"
                          "-fPIC" "-DOPENSSL_IA32_SSE2"
                          (string-append
                           "pregenerated/tmp/"
                           (string-drop (prefix script) 13) "-win32n.asm")))
                '("crypto/fipsmodule/aes/asm/aesni-x86.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-x86.pl"
                  "crypto/fipsmodule/bn/asm/x86-mont.pl"
                  "crypto/chacha/asm/chacha-x86.pl"
                  "crypto/fipsmodule/modes/asm/ghash-x86.pl"))

               (for-each
                (lambda (script)
                  (invoke "perl" script "elf"
                          (string-append (prefix script) "-elf.S"))
                  (invoke "perl" script "macosx"
                          (string-append (prefix script) "-macosx.S"))
                  (invoke "perl" script "nasm"
                          (string-append
                           "pregenerated/tmp/"
                           (string-drop (prefix script) 13) "-nasm.asm")))
                '("crypto/chacha/asm/chacha-x86_64.pl"
                  "crypto/fipsmodule/aes/asm/aesni-x86_64.pl"
                  "crypto/fipsmodule/aes/asm/vpaes-x86_64.pl"
                  "crypto/fipsmodule/bn/asm/x86_64-mont.pl"
                  "crypto/fipsmodule/bn/asm/x86_64-mont5.pl"
                  "crypto/fipsmodule/ec/asm/p256-x86_64-asm.pl"
                  "crypto/fipsmodule/modes/asm/aesni-gcm-x86_64.pl"
                  "crypto/fipsmodule/modes/asm/ghash-x86_64.pl"
                  "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                  "crypto/cipher_extra/asm/chacha20_poly1305_x86_64.pl"))

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "elf" "pregenerated/sha256-x86_64-elf.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "macosx" "pregenerated/sha256-x86_64-macosx.S")

               (invoke "perl" "crypto/fipsmodule/sha/asm/sha512-x86_64.pl"
                       "nasm" "pregenerated/tmp/sha256-x86_64-nasm.asm")

               ;; TODO: Extract ring_core_generated/prefix_symbols_nasm.inc
               ;; and ring_core_generated/prefix_symbols_asm.h from build.rs.

               (for-each
                (lambda (script)
                  (invoke "nasm" "-o" (string-append (prefix script) "o")
                          "-f" "win32" "-i" "include/" "-i" "pregenerated/tmp/"
                          "-Xgnu" "-gcv8" script))
                (find-files "pregenerated/tmp" "win32n\\.asm"))

               (for-each
                (lambda (script)
                  (invoke "nasm" "-o" (string-append (prefix script) "o")
                          "-f" "win64" "-i" "include/" "-i" "pregenerated/tmp/"
                          "-Xgnu" "-gcv8" script))
                (find-files "pregenerated/tmp" "nasm\\.asm")))))))
     (native-inputs (list clang go gzip nasm perl python-minimal tar))
     (propagated-inputs (cargo-inputs 'rust-ring-0.17))
     (home-page "https://github.com/briansmith/ring")
     (synopsis "Safe, fast, small crypto using Rust")
     (description "This package provided safe, fast, small crypto using Rust.")
     (license (list license:isc license:openssl)))))

(define-public rust-rustc-demangle-capi-0.1
  (hidden-package
   (package
     (name "rust-rustc-demangle-capi")
     (version "0.1.0")
     (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle-capi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1s2g4z1yrh1sxl4qkmpd19ss3x2lr9115vbir7pnhgy63r1d63yv"))))
     (build-system cargo-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-c-library
             (lambda _
               (install-file
                (car (find-files "." "^rustc_demangle\\.h$"))
                (string-append #$output "/include"))
               (install-file
                (car (find-files "." "^librustc_demangle.so$"))
                (string-append #$output "/lib")))))))
     (inputs (cargo-inputs 'rust-rustc-demangle-capi-0.1))
     (home-page "https://github.com/alexcrichton/rustc-demangle")
     (synopsis "C API for the @code{rustc-demangle} crate")
     (description "This package provides a C API library for the
@code{rustc-demangle} crate.")
     (license (list license:expat license:asl2.0)))))

(define-public rust-smithay-for-niri
  (let ((commit "ede27079f45eeb7c21796e22f3bc25b741b024ea")
        (revision "2"))
    (hidden-package
     (package
       (name "rust-smithay")
       (version (git-version "0.6.0" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/Smithay/smithay")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "187vdwhy6fjjym9xmyzdhpw1r5xfdfsbbn7y8xv0x9dv12f1var2"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("smithay" "smithay-drm-extras")))
       (inputs (cargo-inputs 'rust-smithay-for-niri))
       (home-page "https://github.com/Smithay/smithay")
       (synopsis "Smithy for Rust Wayland compositors")
       (description
        "Smithay aims to provide building blocks to create wayland compositors
in Rust.  While not being a full-blown compositor, it'll provide objects and
interfaces implementing common functionalities that pretty much any compositor
will need, in a generic fashion.

It supports the @code{wayland}, @code{wayland-protocols}, and some external
extensions, such as @code{wlr-protocols} and @code{plasma-wayland-protocols}.")
       (license license:expat)))))

(define inspired-github-color-scheme-for-rust-syntect-5
  (let ((version "1.3.0"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/sethlopez/InspiredGitHub.tmtheme")
            (commit (string-append "v" version))))
      (file-name "inspired-github-color-scheme-checkout")
      (sha256
       (base32
        "0w2sswa2kid1jwqy28xqvjav17xzkza32i9vvyj67m1kfm3dd6ww")))))

(define solarized-for-rust-syntect-5
  (let ((version "1.5.11"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/braver/Solarized")
            (commit version)))
      (file-name "solarized-checkout")
      (sha256
       (base32
        "05n8wq7zahydrnx36k7awqjz8svn13xsxcazyj0909h4akbsglj1")))))

(define spacegray-for-rust-syntect-5
  (let ((commit "2703e93f559e212ef3895edd10d861a4383ce93d"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/SublimeText/Spacegray")
            (commit commit)))
      (file-name "spacegray-checkout")
      (sha256
       (base32
        "0vzs9i3sdh6f1b25vdbxwyphmxzbqixrnjlgws56fzfngy4my9dj")))))

(define-public rust-dap-types-0.0.1.b40956a
  (let ((commit "b40956a7f4d1939da67429d941389ee306a3a308")
        (revision "0"))
    (package
      (name "rust-dap-types")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/zed-industries/dap-types")
          (commit commit)))
        (file-name
         (git-file-name "rust-dap-types" version))
        (sha256
         (base32 "1dp6w4p2aw37zr1s4mn10chcg0j0clprjmjrbifc3m3qiz9h5zgi"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:install-source? #t
              #:cargo-package-crates ''("dap-types")
              #:phases
              #~(modify-phases %standard-phases
                  (add-before 'build 'generate
                    (lambda _
                      (invoke "cargo" "run" "-p" "generator"))))))
       (inputs (cargo-inputs 'rust-dap-types-0.0.1.b40956a))
       (home-page "https://github.com/zed-industries/dap-types/")
       (synopsis "Rust types for the Debug Adapter Protocol")
       (description
        "This package provides Rust types for the Debug Adapter Protocol.")
       (license (list license:asl2.0 license:expat)))))

(define-public rust-jj-0.29.0.e18eb8e
  (hidden-package
   (package
     (name "rust-jj")
     (version "0.29.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/jj-vcs/jj")
          (commit "e18eb8e05efaa153fad5ef46576af145bba1807f")))
        (file-name
         (git-file-name "rust-jj" "0.29.0.e18eb8e"))
        (sha256
         (base32 "01j8gwa5z7zdcwpzwib4dwgqfgbbip94zby4v0rpx91skscs3w7q"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("jj-lib-proc-macros"
                                      "jj-lib"
                                        ; TODO: Others
                                      )
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'patch
                  (lambda _
                    (substitute* "lib/Cargo.toml"
                      ;; Requires "work in progress" BSER encoding.
                      ;; I'd rather not.
                      (("^watchman.*")
                       "")
                      ;; Would be a dev-dependency and an entirely different
                      ;; git-compatible SCM.
                      (("^sapling-.*")
                       "\n")))))))
     (inputs (cargo-inputs 'rust-jj-0.29.0.e18eb8e))
     (home-page "https://github.com/jj-vcs/jj")
     (synopsis "Git frontend for humans")
     (description
      "This package provides a git frontend for humans.")
     (license license:expat))))

(define-public rust-pet-0.1.0.845945b
  (hidden-package
   (package
     (name "rust-pet")
     (version "0.1.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/microsoft/python-environment-tools.git")
          (commit "845945b830297a50de0e24020b980a65e4820559")))
        (file-name
         (git-file-name "rust-pet" "0.1.0.845945b"))
        (sha256
         (base32 "1r9r0wwc0qwxyksr4l98sb9q64nab1bmcw7gdng8pk8bshyg4lhd"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("pet-fs"
                                      "pet-core"
                                      "pet-python-utils"
                                      "pet-jsonrpc"
                                      "pet-reporter"
                                      "pet-conda"
                                      "pet-virtualenv"
                                      "pet-virtualenvwrapper"
                                      "pet-env-var-path"
                                      "pet-global-virtualenvs"
                                      "pet-homebrew"
                                      "pet-linux-global-python"
                                      "pet-mac-commandlinetools"
                                      "pet-mac-python-org"
                                      "pet-mac-xcode"
                                      "pet-pipenv"
                                      "pet-pixi"
                                      "pet-poetry"
                                      "pet-pyenv"
                                      "pet-telemetry"
                                      "pet-venv"
                                      "pet-windows-store"
                                      "pet-windows-registry"
                                      "pet")
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'fix-versions
                  (lambda _
                    (let ((version #$(package-version this-package)))
                      ;; Depending on random versions is a great idea.  Not.
                      (substitute* (find-files "." "^Cargo[.]toml$")
                        (("^(pet-[a-z-]*) = [{] path = \"([^\"]*)\" [}]" x pkg path)
                         (string-append pkg " = { version = \"" version "\", path = \"" path "\" }")))))))))
     (inputs (cargo-inputs 'rust-pet))
     (home-page "https://github.com/microsoft/python-environment-tools")
     (synopsis "Python environment tools")
     (description
      "This package provides Python environment tools.")
     (license license:expat))))

(define-public rust-runtimed-0.25.0.7130c80
  (hidden-package
   (package
     (name "rust-runtimed")
     (version "0.25.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/ConradIrwin/runtimed")
          (commit "7130c804216b6914355d15d0b91ea91f6babd734")))
        (file-name
         (git-file-name "rust-runtimed" "0.25.0.7130c80"))
        (sha256
         (base32 "1fx4gziw28viw63v3w078q7psmd7jmgi0j79sx4x0kqq4s0szrmz"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("jupyter-protocol"
                                      "runtimelib"
                                      "jupyter-websocket-client"
                                      "nbformat")))
     (inputs (cargo-inputs 'rust-runtimed-for-zed))
     (home-page "https://github.com/ConradIrwin/runtimed")
     (synopsis "Jupyter notebook")
     (description
      "This package provides Jupyter notebook support.")
     (license license:bsd-3))))

(define-public rust-syntect-5
  (hidden-package
   (package
     (name "rust-syntect")
     (version "5.2.0")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/trishume/syntect")
                     (commit (string-append "v" version))))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1wr5x6jy53s597j7kfyzhwph1d07a18qc45s47cx4f399f0xwk9l"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   (delete-file-recursively "scripts")
                   (for-each
                    (lambda (file)
                      (delete-file file)
                      (with-output-to-file file
                        (const (display "\n"))))
                    (find-files "assets" "dump$"))))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:cargo-package-crates ''("syntect")
            #:phases
            #~(modify-phases %standard-phases
                (replace 'build
                  (lambda _
                    (substitute* "Makefile"
                      (("git submodule.*") ""))
                    (with-directory-excursion "testdata"
                      (rmdir "InspiredGitHub.tmtheme")
                      (copy-recursively
                       #+(this-package-native-input
                          "inspired-github-color-scheme-checkout")
                       "InspiredGitHub.tmtheme")
                      (rmdir "Solarized")
                      (copy-recursively
                       #+(this-package-native-input "solarized-checkout")
                       "Solarized")
                      (rmdir "spacegray")
                      (copy-recursively
                       #+(this-package-native-input "solarized-checkout")
                       "spacegray"))
                    (invoke "make" "assets"))))))
     (native-inputs
      (list pkg-config
            inspired-github-color-scheme-for-rust-syntect-5
            solarized-for-rust-syntect-5
            spacegray-for-rust-syntect-5))
     (inputs (cons oniguruma (cargo-inputs 'rust-syntect-5)))
     (home-page "https://github.com/trishume/syntect")
     (synopsis "Library for syntax highlighting and code intelligence")
     (description
      "This package provides a library for syntax highlighting and code
intelligence.")
     (license license:expat))))

(define-public rust-web-view-for-alfis
  (let ((commit "82d7cbce6228b1a964673cc0f22944ad808eab42")
        (revision "0"))
    (hidden-package
     (package
       (name "rust-web-view")
       (version (git-version "0.7.3" revision commit))
       (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url "https://github.com/Boscop/web-view")
                       (commit commit)))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32
                   "1cl65wabbx9cd97qdmbq22d4whqrdsfykm8pbafh67srqjj1qlvr"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("webview-sys" "web-view")))
       (inputs (cargo-inputs 'rust-web-view-for-alfis))
       (home-page "https://github.com/Boscop/web-view")
       (synopsis "Rust bindings for webview.")
       (description
        "This library provides a Rust binding to the original implementation of
webview, a tiny cross-platform library to render web-based GUIs as desktop
applications.")
       (license license:expat)))))

(define-public rust-xim-0.4.0.d50d461
  (hidden-package
   (package
     (name "rust-xim")
     (version "0.4.0")
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/XDeme1/xim-rs")
          (commit "d50d461764c2213655cd9cf65a0ea94c70d3c4fd")))
        (file-name
         (git-file-name "rust-xim" "0.4.0.d50d461"))
        (sha256
         (base32 "1bp91ykfn1c8i8x8ajhma6v67xx1cskk2si2d7rxcc1a38h9lz05"))))
     (build-system cargo-build-system)
     (arguments
      (list #:skip-build? #t
            #:install-source? #t
            #:cargo-package-crates ''("xim-ctext"
                                      "xim-gen"
                                      "xim-parser"
                                      "xim")))
     (inputs (cargo-inputs 'rust-xim-for-zed))
     (home-page "https://github.com/XDeme1/xim-rs")
     (synopsis "X input method client and server")
     (description
      "This package provides a library for @code{xim}, the X input method.")
     (license license:expat))))
