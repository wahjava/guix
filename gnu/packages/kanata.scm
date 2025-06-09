;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages kanata)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crates-apple))

(define-public rust-stretch-0.3
  (package
    (name "rust-stretch")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stretch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11vdmli145j6yakgr7hkzgbnz1kqsb9rq3zrxl1g6dz11k9cc3bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libm" ,rust-libm-0.1))))
    (home-page "https://github.com/vislyhq/stretch")
    (synopsis "High performance & cross-platform Flexbox implementation")
    (description
     "This package provides High performance & cross-platform Flexbox implementation.")
    (license license:asl2.0)))

(define-public rust-newline-converter-0.2
  (package
    (name "rust-newline-converter")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "newline-converter" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03y000bbxnwzb7aipxyw7gm68b1bd8dv7illz03l4qw7bjfx0w8z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-segmentation" ,rust-unicode-segmentation-1))))
    (home-page
     "https://github.com/spitfire05/rnc/tree/master/crates/newline-converter")
    (synopsis "Newline byte converter library")
    (description "This package provides Newline byte converter library.")
    (license license:expat)))

(define-public rust-muldiv-0.2
  (package
    (name "rust-muldiv")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "muldiv" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "014jlry2l2ph56mp8knw65637hh49q7fmrraim2bx9vz0a638684"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sdroege/rust-muldiv")
    (synopsis
     "Provides a trait for numeric types to perform combined multiplication and
division with overflow protection")
    (description
     "This package provides a trait for numeric types to perform combined
multiplication and division with overflow protection.")
    (license license:expat)))

(define-public rust-native-windows-gui-1
  (package
    (name "rust-native-windows-gui")
    (version "1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "native-windows-gui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m44lbslzvs04i4rgcphld23qlwf9zzlzmspgimyp3gnd6k06w2g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-muldiv" ,rust-muldiv-0.2)
                       ("rust-newline-converter" ,rust-newline-converter-0.2)
                       ("rust-plotters" ,rust-plotters-0.3)
                       ("rust-plotters-backend" ,rust-plotters-backend-0.3)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
                       ("rust-stretch" ,rust-stretch-0.3)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-build" ,rust-winapi-build-0.1))))
    (home-page "https://github.com/gabdube/native-windows-gui")
    (synopsis
     "rust library to develop native GUI applications on the desktop for Microsoft Windows. Native-windows-gui wraps the native win32 window controls in a rustic API")
    (description
     "This package provides a rust library to develop native GUI applications on the
desktop for Microsoft Windows.  Native-windows-gui wraps the native win32 window
controls in a rustic API.")
    (license license:expat)))

(define-public rust-karabiner-driverkit-0.1
  (package
    (name "rust-karabiner-driverkit")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "karabiner-driverkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pqnh9n3a8wxqzdj7d30f99g322da8zpnixsq5gfs9n1klccj380"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-os-info" ,rust-os-info-3))))
    (home-page "https://github.com/Psych3r/driverkit")
    (synopsis
     "Minimal Karabiner-DriverKit-VirtualHIDDevice wrapper for kanata")
    (description
     "This package provides Minimal Karabiner-@code{DriverKit-VirtualHIDDevice} wrapper for kanata.")
    (license license:lgpl3)))

(define-public rust-kanata-tcp-protocol-0.180
  (package
    (name "rust-kanata-tcp-protocol")
    (version "0.180.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-tcp-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x74ncvffz3cani6l84jl8rqr26d445hz3h88h75k7aa59jc8fax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "")
    (synopsis "TCP protocol for kanata. This does not follow semver")
    (description
     "This package provides TCP protocol for kanata.  This does not follow semver.")
    (license license:lgpl3)))

(define-public rust-patricia-tree-0.8
  (package
    (name "rust-patricia-tree")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "patricia_tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s5fya6rvgg2gxxp5mbv0xdq8jqikps1sc6snk23zrgzkd9z9wii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/sile/patricia_tree")
    (synopsis "Memory-efficient data structures based on patricia tree")
    (description
     "This package provides Memory-efficient data structures based on patricia tree.")
    (license license:expat)))

(define-public rust-kanata-parser-0.180
  (package
    (name "rust-kanata-parser")
    (version "0.180.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dfbjc2gy5jc3wqy0fnn6c9wpqxcwwwkkv6lf4lgnp6sfkvqsi18"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("cmd")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bytemuck" ,rust-bytemuck-1)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-kanata-keyberon" ,rust-kanata-keyberon-0.180)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-patricia-tree" ,rust-patricia-tree-0.8)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis
     "parser for configuration language of kanata, a keyboard remapper.")
    (description
     "This package provides a parser for configuration language of kanata, a keyboard
remapper.")
    (license license:lgpl3)))

(define-public rust-kanata-keyberon-macros-0.2
  (package
    (name "rust-kanata-keyberon-macros")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-keyberon-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lj7ldiazmszh0k01h7mjzhjg59bdakvx2pnpc9mq2ir0czzixkk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "")
    (synopsis "Macros for keyberon. Fork for kanata project")
    (description
     "This package provides Macros for keyberon.  Fork for kanata project.")
    (license license:expat)))

(define-public rust-kanata-keyberon-0.180
  (package
    (name "rust-kanata-keyberon")
    (version "0.180.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-keyberon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b0axjmzq79pi5xbj82c38mvvwwylar42jwiwzz3n8v0bjln6czj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arraydeque" ,rust-arraydeque-0.5)
                       ("rust-heapless" ,rust-heapless-0.7)
                       ("rust-kanata-keyberon-macros" ,rust-kanata-keyberon-macros-0.2)
                       ("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/TeXitoi/keyberon")
    (synopsis "Pure Rust keyboard firmware. Fork intended for use with kanata")
    (description
     "This package provides Pure Rust keyboard firmware.  Fork intended for use with kanata.")
    (license license:expat)))

(define-public rust-num-enum-derive-0.6
  (package
    (name "rust-num-enum-derive")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num_enum_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19k57c0wg56vzzj2w77jsi8nls1b8xh8pvpzjnrgf8d9cnvpsrln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/illicitonion/num_enum")
    (synopsis
     "Internal implementation details for ::num_enum (Procedural macros to make inter-operation between primitives and enums easier)")
    (description
     "This package provides Internal implementation details for ::num_enum (Procedural macros to make
inter-operation between primitives and enums easier).")
    (license (list license:bsd-3 license:expat license:asl2.0))))

(define-public rust-num-enum-0.6
  (package
    (name "rust-num-enum")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num_enum" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18bna04g6zq978z2b4ygz0f8pbva37id4xnpgwh8l41w1m1mn0bs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-enum-derive" ,rust-num-enum-derive-0.6))))
    (home-page "https://github.com/illicitonion/num_enum")
    (synopsis
     "Procedural macros to make inter-operation between primitives and enums easier")
    (description
     "This package provides Procedural macros to make inter-operation between primitives and enums easier.")
    (license (list license:bsd-3 license:expat license:asl2.0))))

(define-public rust-interception-sys-0.1
  (package
    (name "rust-interception-sys")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "interception-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lgwbml7gzq5a5rriy708w68gx6yiw9cdg7xy2c5vsrrck7pbs5b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/bozbez/interception-sys")
    (synopsis "FFI bindings for Interception")
    (description "This package provides FFI bindings for Interception.")
    (license license:lgpl3)))

(define-public rust-kanata-interception-0.3
  (package
    (name "rust-kanata-interception")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata-interception" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01mn1dskhm124x0nxfcw5cyb07h0i256x9bfj23aq6adjsdpprg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-interception-sys" ,rust-interception-sys-0.1)
                       ("rust-num-enum" ,rust-num-enum-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Safe wrapper for Interception. Forked for use with kanata")
    (description
     "This package provides Safe wrapper for Interception.  Forked for use with kanata.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wayland-protocols-wlr-0.3
  (package
    (name "rust-wayland-protocols-wlr")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols-wlr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gwbd9nv71ahaqylfm2lvml5bwl6z0ygwdy40ijy1h4r6g3wvdhw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.32)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-server" ,rust-wayland-server-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the WLR wayland protocol extensions")
    (description
     "This package provides Generated API for the WLR wayland protocol extensions.")
    (license license:expat)))

(define-public rust-wayland-server-0.31
  (package
    (name "rust-wayland-server")
    (version "0.31.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11apclvwsp43l24gvdlrg455b7pr2nnfcd2xc8s0vahdry6gnpa8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-downcast-rs" ,rust-downcast-rs-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol, server side")
    (description
     "This package provides Bindings to the standard C implementation of the wayland protocol, server side.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.32
  (package
    (name "rust-wayland-protocols")
    (version "0.32.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aj2209i962k1www23wb7zdgx81y6a35ilgyjhbm56hy9r2pb43p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31)
                       ("rust-wayland-server" ,rust-wayland-server-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the officials wayland protocol extensions")
    (description
     "This package provides Generated API for the officials wayland protocol extensions.")
    (license license:expat)))

(define-public rust-wayland-scanner-0.31
  (package
    (name "rust-wayland-scanner")
    (version "0.31.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "110ldnyfxjqvjssir1jf3ndlci7xy9lpv4aqg775y518bpyxlvw9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quick-xml" ,rust-quick-xml-0.37)
                       ("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Wayland Scanner for generating rust APIs from XML wayland protocol files")
    (description
     "This package provides Wayland Scanner for generating rust APIs from XML wayland protocol files.")
    (license license:expat)))

(define-public rust-wayland-client-0.31
  (package
    (name "rust-wayland-client")
    (version "0.31.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qcsgnfvgyxpvda70ww2836p8j8pd4jwll7km7bdniq8gg3ag3wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-scanner" ,rust-wayland-scanner-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol, client side")
    (description
     "This package provides Bindings to the standard C implementation of the wayland protocol, client side.")
    (license license:expat)))

(define-public rust-wayland-sys-0.31
  (package
    (name "rust-wayland-sys")
    (version "0.31.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05b6i4lg2qrrz7l4h2b5fd7blkkvxq34i1yvlngsmmbpkhwvpknv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dlib" ,rust-dlib-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memoffset" ,rust-memoffset-0.9)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "FFI bindings to the various libwayland-*.so libraries. You should only need this crate if you are working on custom wayland protocol extensions. Look at the crate wayland-client for usable bindings")
    (description
     "This package provides FFI bindings to the various libwayland-*.so libraries.  You should only need
this crate if you are working on custom wayland protocol extensions.  Look at
the crate wayland-client for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-backend-0.3
  (package
    (name "rust-wayland-backend")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-backend" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08a1l8ya2siwvr9jhdz43nmmm7brnw848zra7sfwfpry8a0h2xzy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-downcast-rs" ,rust-downcast-rs-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.5)
                       ("rust-raw-window-handle" ,rust-raw-window-handle-0.6)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-wayland-sys" ,rust-wayland-sys-0.31))))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Low-level bindings to the Wayland protocol")
    (description
     "This package provides Low-level bindings to the Wayland protocol.")
    (license license:expat)))

(define-public rust-tree-magic-mini-3
  (package
    (name "rust-tree-magic-mini")
    (version "3.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tree_magic_mini" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qwx2b0xfr00vdskl951cvh3m040zj5n8vm7ln4k6p143ybyiida"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fnv" ,rust-fnv-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-petgraph" ,rust-petgraph-0.6)
                       ("rust-tree-magic-db" ,rust-tree-magic-db-3))))
    (home-page "https://github.com/mbrubeck/tree_magic/")
    (synopsis
     "Determines the MIME type of a file by traversing a filetype tree")
    (description
     "This package provides Determines the MIME type of a file by traversing a filetype tree.")
    (license license:expat)))

(define-public rust-tempfile-3
  (package
    (name "rust-tempfile")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tempfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18fnp7mjckd9c9ldlb2zhp1hd4467y2hpvx9l50j97rlhlwlx9p8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fastrand" ,rust-fastrand-2)
                       ("rust-getrandom" ,rust-getrandom-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustix" ,rust-rustix-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://stebalien.com/projects/tempfile-rs/")
    (synopsis "library for managing temporary files and directories.")
    (description
     "This package provides a library for managing temporary files and directories.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix-0.38
  (package
    (name "rust-rustix")
    (version "0.38.44")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustix" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-errno" ,rust-errno-0.3)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.59))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls")
    (description
     "This package provides Safe Rust bindings to POSIX/Unix/Linux/Winsock-like syscalls.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "log" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-sval" ,rust-sval-2)
                       ("rust-sval-ref" ,rust-sval-ref-2)
                       ("rust-value-bag" ,rust-value-bag-1))))
    (home-page "https://github.com/rust-lang/log")
    (synopsis "lightweight logging facade for Rust")
    (description
     "This package provides a lightweight logging facade for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.172")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ykz4skj7gac14znljm5clbnrhini38jkq3d60jggx3y5w2ayl6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.")
    (description
     "This package provides Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wl-clipboard-rs-0.9
  (package
    (name "rust-wl-clipboard-rs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wl-clipboard-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sxsaspzix3xiq6wi1l1g55acgi04sv6r7gxz94zar80wv8ghpwf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-2)
                       ("rust-tree-magic-mini" ,rust-tree-magic-mini-3)
                       ("rust-wayland-backend" ,rust-wayland-backend-0.3)
                       ("rust-wayland-client" ,rust-wayland-client-0.31)
                       ("rust-wayland-protocols" ,rust-wayland-protocols-0.32)
                       ("rust-wayland-protocols-wlr" ,rust-wayland-protocols-wlr-0.3))))
    (home-page "https://github.com/YaLTeR/wl-clipboard-rs")
    (synopsis
     "Access to the Wayland clipboard for terminal and other window-less applications")
    (description
     "This package provides Access to the Wayland clipboard for terminal and other window-less applications.")
    (license (list license:expat license:asl2.0))))

(define-public rust-objc2-uniform-type-identifiers-0.3
  (package
    (name "rust-objc2-uniform-type-identifiers")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-uniform-type-identifiers" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "118wd3r2gm72vbdcbpyz877r4rzc5k3abm65cdwrm2dmg1bshr6r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the UniformTypeIdentifiers framework")
    (description
     "This package provides Bindings to the @code{UniformTypeIdentifiers} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-quartz-core-0.3
  (package
    (name "rust-objc2-quartz-core")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-quartz-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mfcnbgs4akjwb2xxqmkfixpz98j1db8hhrkck4js62zrnhbdzwh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the QuartzCore/CoreAnimation framework")
    (description
     "This package provides Bindings to the @code{QuartzCore/CoreAnimation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-video-0.3
  (package
    (name "rust-objc2-core-video")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-video" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00v3zcxl0xv8q964888il96d23nh3jbg9rm91fmqr5vydkkw728r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreVideo framework")
    (description
     "This package provides Bindings to the @code{CoreVideo} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-ml-0.3
  (package
    (name "rust-objc2-core-ml")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-ml" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11kiimdf1qd4cb7hs59qkv6y1yqda16rx540v28bjvwy7x4vgfj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreML framework")
    (description
     "This package provides Bindings to the @code{CoreML} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-image-0.3
  (package
    (name "rust-objc2-core-image")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zh0ihcb46lh09azhr5hfn74rx576hdmjmy1477nqsrqqh6drcvr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-ml" ,rust-objc2-core-ml-0.3)
                       ("rust-objc2-core-video" ,rust-objc2-core-video-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreImage framework")
    (description
     "This package provides Bindings to the @code{CoreImage} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-metal-0.3
  (package
    (name "rust-objc2-metal")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-metal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x588xxxlsp4b061vgbmj4jx8h10mcspnic22ymhlm1r68c6q93z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Metal framework")
    (description "This package provides Bindings to the Metal framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-io-surface-0.3
  (package
    (name "rust-objc2-io-surface")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-io-surface" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g0c89swz8hgfrh0j1iqhcz3ig7cyhavn3p9gi2s77sjjanfk0kj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the IOSurface framework")
    (description "This package provides Bindings to the IOSurface framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-graphics-0.3
  (package
    (name "rust-objc2-core-graphics")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "197mf2a4yvvigkd9hsp8abbpip7rn3mmc55psv1ba89hq5l6r74q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-io-surface" ,rust-objc2-io-surface-0.3)
                       ("rust-objc2-metal" ,rust-objc2-metal-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreGraphics framework")
    (description
     "This package provides Bindings to the @code{CoreGraphics} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-data-0.3
  (package
    (name "rust-objc2-core-data")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "179j6k4a7m5780qn41xnsh7p0x4w4gvpqhc6hs7531wjsbvvn7r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-cloud-kit" ,rust-objc2-cloud-kit-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreData framework")
    (description
     "This package provides Bindings to the @code{CoreData} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-security-0.3
  (package
    (name "rust-objc2-security")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-security" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i0sfdwjfynwvf5vpbfqpdsabh3fp96v7p244v20hsxn7bpy1y71"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Security framework")
    (description "This package provides Bindings to the Security framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-services-0.3
  (package
    (name "rust-objc2-core-services")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-services" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0r6n0qdf0337l12y1kciaq726m8qlwmam3brdlipsv1nrpjfd6qm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-security" ,rust-objc2-security-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreServices framework")
    (description
     "This package provides Bindings to the @code{CoreServices} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-foundation-0.3
  (package
    (name "rust-objc2-core-foundation")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreFoundation framework")
    (description
     "This package provides Bindings to the @code{CoreFoundation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-foundation-0.3
  (package
    (name "rust-objc2-foundation")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-foundation" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-services" ,rust-objc2-core-services-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Foundation framework")
    (description "This package provides Bindings to the Foundation framework.")
    (license license:expat)))

(define-public rust-objc2-contacts-0.3
  (package
    (name "rust-objc2-contacts")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-contacts" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gdp7mrip1dfar97drqkkmmyarkzwn015v501swrn233ljcbs58a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the Contacts framework")
    (description "This package provides Bindings to the Contacts framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-dispatch2-0.3
  (package
    (name "rust-dispatch2")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "dispatch2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings and wrappers for Apple's Grand Central Dispatch (GCD)")
    (description
     "This package provides Bindings and wrappers for Apple's Grand Central Dispatch (GCD).")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-core-location-0.3
  (package
    (name "rust-objc2-core-location")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-core-location" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0h2p2cpd16pghqzr15nzznva19r6fkdvbfs3hihrvajq4mwpa3xc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block2" ,rust-block2-0.6)
                       ("rust-dispatch2" ,rust-dispatch2-0.3)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-contacts" ,rust-objc2-contacts-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CoreLocation framework")
    (description
     "This package provides Bindings to the @code{CoreLocation} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-cloud-kit-0.3
  (package
    (name "rust-objc2-cloud-kit")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-cloud-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pd1iq5gw1c024gipy3x7al8p40g2p8b3pqp27zyc4dlv7f4yq8p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-core-location" ,rust-objc2-core-location-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the CloudKit framework")
    (description
     "This package provides Bindings to the @code{CloudKit} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-block2-0.6
  (package
    (name "rust-block2")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wnwha7wjjqiamj9abq5l45fyzdxna2k2la0rp9w2hravc5jy39l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2" ,rust-objc2-0.6))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Apple's C language extension of blocks")
    (description
     "This package provides Apple's C language extension of blocks.")
    (license license:expat)))

(define-public rust-objc2-app-kit-0.3
  (package
    (name "rust-objc2-app-kit")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-app-kit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1k4vz0s63rpp1yyhx96mh9nndn1zzv2cwxzpvw6rnigcidb9zwp6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-block2" ,rust-block2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-cloud-kit" ,rust-objc2-cloud-kit-0.3)
                       ("rust-objc2-core-data" ,rust-objc2-core-data-0.3)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-core-image" ,rust-objc2-core-image-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-objc2-quartz-core" ,rust-objc2-quartz-core-0.3)
                       ("rust-objc2-uniform-type-identifiers" ,rust-objc2-uniform-type-identifiers-0.3))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Bindings to the AppKit framework")
    (description
     "This package provides Bindings to the @code{AppKit} framework.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-proc-macros-0.2
  (package
    (name "rust-objc2-proc-macros")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-proc-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zi9y5bb6igas980il7x0d5wijy959v69hhzzffmf17fii5h6hkw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Procedural macros for the objc2 project")
    (description
     "This package provides Procedural macros for the objc2 project.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-exception-helper-0.1
  (package
    (name "rust-objc2-exception-helper")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-exception-helper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12nrg6fhhp2rzmnym6s37h7w9v9sa9wbaixvfsq3axrdnzxwb8f7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "External helper function for catching Objective-C exceptions")
    (description
     "This package provides External helper function for catching Objective-C exceptions.")
    (license (list license:zlib license:asl2.0 license:expat))))

(define-public rust-objc2-encode-4
  (package
    (name "rust-objc2-encode")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2-encode" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Objective-C type-encoding representation and parsing")
    (description
     "This package provides Objective-C type-encoding representation and parsing.")
    (license license:expat)))

(define-public rust-objc2-0.6
  (package
    (name "rust-objc2")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l85a8r77i8i183fqyx55kqm2nh9rzg2z3z59kjb4fj92iz5kil8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-objc2-encode" ,rust-objc2-encode-4)
                       ("rust-objc2-exception-helper" ,rust-objc2-exception-helper-0.1)
                       ("rust-objc2-proc-macros" ,rust-objc2-proc-macros-0.2))))
    (home-page "https://github.com/madsmtm/objc2")
    (synopsis "Objective-C interface and runtime bindings")
    (description
     "This package provides Objective-C interface and runtime bindings.")
    (license license:expat)))

(define-public rust-arboard-3
  (package
    (name "rust-arboard")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "arboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w1yqcx51153hy5w3y0702xjc9nmlhncw9f5l0rdwbl62pvj3py1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clipboard-win" ,rust-clipboard-win-5)
                       ("rust-image" ,rust-image-0.25)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-objc2" ,rust-objc2-0.6)
                       ("rust-objc2-app-kit" ,rust-objc2-app-kit-0.3)
                       ("rust-objc2-core-foundation" ,rust-objc2-core-foundation-0.3)
                       ("rust-objc2-core-graphics" ,rust-objc2-core-graphics-0.3)
                       ("rust-objc2-foundation" ,rust-objc2-foundation-0.3)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-windows-sys" ,rust-windows-sys-0.59)
                       ("rust-wl-clipboard-rs" ,rust-wl-clipboard-rs-0.9)
                       ("rust-x11rb" ,rust-x11rb-0.13))))
    (home-page "https://github.com/1Password/arboard")
    (synopsis "Image and text handling for the OS clipboard")
    (description
     "This package provides Image and text handling for the OS clipboard.")
    (license (list license:expat license:asl2.0))))

(define-public rust-kanata-1
  (package
    (name "kanata")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "kanata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1632iaclw9qy6sswm2wqapa28px7rdxqchk8b1wwp6k2scysr2bs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("cmd")
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-arboard" ,rust-arboard-3)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-core-graphics" ,rust-core-graphics-0.24)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-embed-resource" ,rust-embed-resource-2)
                       ("rust-encode-unicode" ,rust-encode-unicode-0.3)
                       ("rust-evdev" ,rust-evdev-0.12)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-indoc" ,rust-indoc-2)
                       ("rust-inotify" ,rust-inotify-0.10)
                       ("rust-instant" ,rust-instant-0.1)
                       ("rust-kanata-interception" ,rust-kanata-interception-0.3)
                       ("rust-kanata-keyberon" ,rust-kanata-keyberon-0.180)
                       ("rust-kanata-parser" ,rust-kanata-parser-0.180)
                       ("rust-kanata-tcp-protocol" ,rust-kanata-tcp-protocol-0.180)
                       ("rust-karabiner-driverkit" ,rust-karabiner-driverkit-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-miette" ,rust-miette-5)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-muldiv" ,rust-muldiv-1)
                       ("rust-native-windows-gui" ,rust-native-windows-gui-1)
                       ("rust-nix" ,rust-nix-0.26)
                       ("rust-objc" ,rust-objc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-open" ,rust-open-5)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-radix-trie" ,rust-radix-trie-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-sd-notify" ,rust-sd-notify-0.4)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-simplelog" ,rust-simplelog-0.12)
                       ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.2)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (home-page "https://github.com/jtroo/kanata")
    (synopsis "Multi-layer keyboard customization")
    (description "This package provides Multi-layer keyboard customization.")
    (license license:lgpl3)))
