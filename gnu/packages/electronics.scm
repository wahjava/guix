;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2022, 2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Juliana Sims <juli@incana.org>
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 Konstantinos Agiannis <agiannis.kon@gmail.com>
;;; Copyright © 2018-2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages electronics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fpga)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public camv-rnd
  (package
    (name "camv-rnd")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/camv-rnd/"
                           "releases/camv-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "1dp1vj5rpxlddx40paa9i727c92is3bz6z6pa0y6dy2nsjcm86fs"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/route-rnd/")
    (synopsis "Viewer for electronic boards in CAM file formats")
    (description
     "@code{Camv-rnd} is a viewer for @acronym{PCB, Printed Circuit Board}
supporting gerber, excellon and g-code.  It is part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public comedilib
  (package
    (name "comedilib")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.comedi.org/download/comedilib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0jdw5gp02d8q3p4ldjrc3zaw0v435kmn3c95pv094gyxj3pwhacm"))))
    (build-system gnu-build-system)
    (synopsis "Library for Comedi")
    (description "Comedilib is a user-space library that provides a
developer-friendly interface to Comedi devices.  Comedi is a collection of
drivers for a variety of common data acquisition plug-in boards.  The drivers
are implemented as a core Linux kernel module providing common functionality and
individual low-level driver modules.")
    (home-page "https://www.comedi.org/")
    (license license:lgpl2.1)))

(define-public json-for-vhdl
  ;; No tagged releases.
  (let ((commit "0dc9e317440263cd4941f157f5e5668baa858ec2")
        (revision "0"))
    (package
      (name "json-for-vhdl")
      (version (git-version "20220905" revision commit)) ;last revision
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/Paebbels/JSON-for-VHDL/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1062g2c3dpsb67zhqrn1j04p7jl28g4mcxd6nhrqqfffjsvxkpw9"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("src" "share/json-for-vhdl"
             #:include ("vhdl")))
        #:phases
        #~(modify-phases %standard-phases
            ;; The examples/Encodings_VUnit test requires vhdl builtins.
            (add-after 'unpack 'fix-check
              (lambda _
                (substitute* "tests/VUnit/run.py"
                  (("from_argv\\(\\)")
                   "from_argv()\nvu.add_vhdl_builtins()"))))
            (add-after 'install 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "python3" "tests/VUnit/run.py" "-v")))))))
      (native-inputs
       (list nvc python-minimal python-vunit))
      (home-page "https://github.com/Paebbels/JSON-for-VHDL/")
      (synopsis "Parse and query JSON data structures in VHDL")
      (description
       "The JSON-for-VHDL library provides a parser to query JSON data
structures from external files on disk.  It provides a context to be
used in the declarative section of design units.")
      (license license:asl2.0))))

(define librnd
  (package
    (name "librnd")
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.repo.hu/projects/librnd/"
                                  "releases/librnd-" version ".tar.bz2"))
              (sha256
               (base32
                "1qjv6gg9fb3rpvr1y9l5nbzz2xk2sa4nqz0dgwvds5hc1bmd97mf"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #false                   ;no check target
      #:phases
      #~(modify-phases %standard-phases
          ;; The configure script doesn't tolerate most of our configure
          ;; flags.
          (replace 'configure
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs
     (list gd glib glu gtk gtkglext libepoxy))
    (native-inputs
     (list pkg-config))
    (home-page "http://repo.hu/projects/librnd/")
    (synopsis "Two-dimensional CAD engine")
    (description "This is a flexible, modular two-dimensional CAD engine
@itemize
@item with transparent multiple GUI toolkit support;
@item a flexible, dynamic menu system;
@item a flexible, dynamic configuration system; and
@item support for user scripting in a dozen languages.
@end itemize")
    (license license:gpl2+)))

(define-public libserialport
  (package
    (name "libserialport")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://sigrok.org/libserialport")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dn10gmm3rwdsiw1psaczb9m52x6cfkfrbywm4f5y8fsmghh7dsy"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (home-page "https://sigrok.org/wiki/Libserialport")
    (synopsis "Library for using serial ports")
    (description "Libserialport is a minimal shared library written in C that is intended
to take care of the OS-specific details when writing software that uses serial ports.")
    (license license:lgpl3+)))

(define-public libsigrok
  (let ((commit "f06f788118191d19fdbbb37046d3bd5cec91adb1")
        (revision "2"))
    (package
      (name "libsigrok")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://sigrok.org/libsigrok")
               (commit commit)))
         (sha256
          (base32 "1ahgpa0gaa4fl8c6frpgamvgxg0fisfwlqddr5x25456vkk2i9zi"))
         (file-name (git-file-name name version))))
      (outputs '("out" "doc"))
      (arguments
       (list
        #:tests? #f                      ; tests need USB access
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'change-udev-group
              (lambda _
                (substitute* (find-files "contrib" "\\.rules$")
                  (("plugdev") "dialout"))))
            (add-after 'build 'build-doc
              (lambda _
                (invoke "doxygen")))
            (add-after 'install 'install-doc
              (lambda _
                (copy-recursively
                 "doxy/html-api"
                 (string-append #$output:doc "/share/doc/libsigrok"))))
            (add-after 'install-doc 'install-udev-rules
              (lambda _
                (for-each
                 (lambda (file)
                   (install-file
                    file
                    (string-append #$output "/lib/udev/rules.d/")))
                          (find-files "contrib" "\\.rules$"))))
            (add-after 'install-udev-rules 'install-fw
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((fx2lafw (assoc-ref inputs "sigrok-firmware-fx2lafw"))
                       (dir-suffix "/share/sigrok-firmware/")
                       (input-dir (string-append fx2lafw dir-suffix))
                       (output-dir (string-append #$output dir-suffix)))
                  (for-each
                   (lambda (file)
                     (install-file file output-dir))
                   (find-files input-dir "."))))))))
      (native-inputs
       (list autoconf automake doxygen graphviz libtool
             sigrok-firmware-fx2lafw pkg-config))
      (inputs
       (list python zlib))
      ;; libsigrokcxx.pc lists "glibmm" in Requires libsigrok.pc lists
      ;; "libserialport", "libusb", "libftdi" and "libzip" in Requires.private
      ;; and "glib" in Requires
      (propagated-inputs
       (list glib
             glibmm-2.66
             libserialport
             libusb
             libftdi
             libzip))
      (build-system gnu-build-system)
      (home-page "https://www.sigrok.org/wiki/Libsigrok")
      (synopsis "Basic hardware access drivers for logic analyzers")
      (description "@code{libsigrok} is a shared library written in C which
provides the basic hardware access drivers for logic analyzers and other
supported devices, as well as input/output file format support.")
      (license license:gpl3+))))

(define-public libsigrokdecode
  (let ((commit "71f451443029322d57376214c330b518efd84f88")
        (revision "1"))
    (package
      (name "libsigrokdecode")
      (version (git-version "0.5.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/libsigrokdecode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11l8vnf2khqbaqas7cfnq3f8q5w7am6nbkkd5mqj5kpb3ya2avb9"))))
      (outputs '("out" "doc"))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-doc
              (lambda _
                (invoke "doxygen")))
            (add-after 'install 'install-doc
              (lambda _
                (copy-recursively
                 "doxy/html-api"
                 (string-append #$output:doc
                                "/share/doc/libsigrokdecode")))))))
      (native-inputs
       (list check doxygen graphviz pkg-config automake autoconf libtool))
      ;; libsigrokdecode.pc lists "python" in Requires.private, and "glib" in
      ;; Requires.
      (propagated-inputs
       (list glib python))
      (build-system gnu-build-system)
      (home-page "https://www.sigrok.org/wiki/Libsigrokdecode")
      (synopsis
       "Library providing (streaming) protocol decoding functionality")
      (description
       "Libsigrokdecode is a shared library written in C, which provides
(streaming) protocol decoding functionality.")
      (license license:gpl3+))))

(define-public m8c
  (package
    (name "m8c")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/laamaa/m8c")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vv0m4ry23nns5a47m2n9k6i3wly2jjc5n1j3l7sh1m480ga3d42"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:configure-flags
      #~(list "-DUSE_LIBSERIALPORT=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libdecor libserialport sdl3))
    (home-page "https://github.com/laamaa/m8c")
    (synopsis "Cross-platform M8 tracker headless client")
    (description
     "The @url{https://dirtywave.com/products/m8-tracker,Dirtywave M8 Tracker}
is a portable sequencer and synthesizer, featuring 8 tracks of assignable
instruments such as FM, waveform synthesis, virtual analog, sample playback, and
MIDI output.  It is powered by a @url{https://www.pjrc.com/teensy/,Teensy}
micro-controller and inspired by the Gameboy tracker
@url{https://www.littlesounddj.com/lsd/index.php,Little Sound DJ}.  m8c is a
client for @url{https://github.com/Dirtywave/M8HeadlessFirmware,M8 Headless}
which allows one to install the M8 firmware on any Teensy.")
    (license (list license:cc-by-sa3.0
                   license:expat
                   license:public-domain
                   license:zlib))))

(define-public openboardview
  (package
    (name "openboardview")
    (version "9.95.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenBoardView/OpenBoardView")
                    (commit version)))
              (file-name (git-file-name name version))
              (modules '((ice-9 ftw)
                         (srfi srfi-26)
                         (guix build utils)))
              (snippet
               '(with-directory-excursion "src"
                  (define keep (list "." ".." "openboardview"))
                  (for-each (lambda (f)
                              (when (eq? 'directory (stat:type (lstat f)))
                                (delete-file-recursively f)))
                            (scandir "." (negate (cut member <> keep))))))
              (patches
               (search-patches "openboardview-use-system-imgui.patch"
                               "openboardview-use-system-mpc.patch"))
              (sha256
               (base32
                "1safjd729a7591rigkiy3c678bivrj5q1qwg1f18sijhlsfkf5b3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build glib-or-gtk-build-system) #:prefix gtk:))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'configure-glad
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/CMakeLists.txt"
                (("add_subdirectory\\(glad\\)")
                 (string-append
                  ;; Configure Glad to use static Khronos XML specifications
                  ;; instead of attempting to fetch them from the Internet.
                  "option(GLAD_REPRODUCIBLE \"Reproducible build\" ON)\n"
                  ;; Use the CMake files from our glad package.
                  "add_subdirectory("
                  (search-input-directory inputs "share/glad") ;source_dir
                  " src/glad)\n")))))                          ;binary dir
          (add-before 'configure 'dynamically-load-gtk-via-absolute-path
            ;; The GTK library is not linked thus not present in the RUNPATH of
            ;; the produced binary; the absolute path of the libraries must to
            ;; the dynamic loader otherwise they aren't found.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/openboardview/unix.cpp"
                (("libgtk-3.so")
                 (search-input-file inputs "lib/libgtk-3.so")))))
          ;; Add the two extra phases from `glib-or-gtk-build-system'.
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list pkg-config
           python
           glad-0.1
           stb-image
           utf8-h))
    (inputs
     (list fontconfig
           gtk+
           ;; OpenBoardView can build with Dear ImGui 1.88, but there are some
           ;; usability problems such as the difficulty to register clicks.
           imgui-1.87
           orangeduck-mpc
           sdl2
           sqlite
           zlib))
    (home-page "https://github.com/OpenBoardView/OpenBoardView")
    (synopsis "Viewer for BoardView files")
    (description "OpenBoardView is a viewer for BoardView files, which present
the details of a printed circuit board (PCB).  It comes with features
such as:
@itemize
@item Dynamic part outline rendering, including complex connectors
@item Annotations, for leaving notes about parts, nets, pins or location
@item Configurable colour themes
@item Configurable DPI to facilitate usage on 4K monitors
@item Configurable for running on slower systems
@item Reads FZ (with key), BRD, BRD2, BDV and BV* formats.
@end itemize")
    (license license:expat)))

(define-public pcb-rnd
  (package
    (name "pcb-rnd")
    (version "3.1.7b")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://repo.hu/projects/pcb-rnd/"
                                  "releases/pcb-rnd-" version ".tar.gz"))
              (sha256
               (base32
                "1djsa0w53l6nvhwv28rlhpva55ir9n3xdvjgnjj8fgvcmrqlzrsl"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/pcb-rnd/")
    (synopsis "Modular layout editor")
    (description "@code{Pcb-rnd} is a @acronym{Printed Circuit Board} layout
editor, part of the RiNgDove EDA suite.")
    (license license:gpl2+)))

(define-public prjtrellis
  ;; The last release is 2 years old; use the latest commit for now.
  (let ((commit "898329dddf6ce6463299973081f109d645b9c55f")
        (revision "0"))
    (package
      (name "prjtrellis")
      (version (git-version "1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/prjtrellis/")
               (commit commit)
               ;; Pull the bitstream database for ECP5 devices; this is useful
               ;; only by prjtrellis: there is no need to package it separately.
               (recursive? #t)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          ;; Remove bundled source code for which Guix has packages.
          '(with-directory-excursion "libtrellis/3rdparty"
             (for-each delete-file-recursively
                       '("pybind11"))))
         (sha256
          (base32 "1qljgn7rxz114vki21rms70zi9rgr4gw7crdfihxx1n68zgv60gg"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:configure-flags
        #~(list (string-append "-DPYBIND11_INCLUDE_DIR="
                               (search-input-directory %build-inputs
                                                       "include/pybind11")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                (chdir "libtrellis"))))))
      (native-inputs (list python))
      (inputs (list openocd boost pybind11))
      (synopsis "Placement and routing for ECP5 FPGAs")
      (description
       "Project Trellis is a Nextpnr backend compatible with ECP5 FPGAs.
The following features are currently available:
@itemize
@item logic slice functionality, including carries
@item distributed RAM inside logic slices
@item all internal interconnect
@item basic IO, including tristate
@item block RAM, using inference or manual instantiation
@item multipliers using manual instantiation
@item global networks and PLLs
@item transcievers (DCUs.)
@end itemize")
      (home-page "https://github.com/YosysHQ/prjtrellis/")
      (license license:expat))))

(define-public opensta
  ;; There are no releases, we use last commit.
  (let ((commit "12f03395ec80d3593f4796b2a3cf5480e75735bd")
        (revision "0"))
    (package
      (name "opensta")
      ;; The version string is taken from the CMakeLists.txt.
      (version (git-version "2.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/parallaxsw/OpenSTA/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gka50p4wv2b49d8jbw5fs3qg7cppa8ynl3diqgdf8mqgskwapzf"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; Tests expect output sta binary inside source tree.
        #:out-of-source? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "../test/regression"))))
            (add-before 'build 'create-build-dir
              (lambda _
                (mkdir-p "./build")
                (chdir "./build"))))
        #:configure-flags
        #~(list
           (string-append "-DCUDD_DIR=" #$(this-package-input "cudd"))
           (string-append "-DBUILD_SHARED_LIBS=YES")
           "-B./build")))
      (native-inputs (list bison flex swig))
      (inputs (list cudd eigen tcl tcllib zlib))
      (synopsis "Parallax Static Timing Analyzer")
      (description
       "OpenSTA is a gate level static timing verifier.  As a stand-alone
executable it can be used to verify the timing of a design using standard file
formats.")
      (home-page "https://github.com/parallaxsw/OpenSTA/")
      (license license:gpl3+))))

(define-public pulseview
  (package
    (name "pulseview")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://sigrok.org/download/source/pulseview/pulseview-"
             version ".tar.gz"))
       (sha256
        (base32
         "1jxbpz1h3m1mgrxw74rnihj8vawgqdpf6c33cqqbyd8v7rxgfhph"))
       (patches (search-patches "pulseview-qt515-compat.patch"
                                "pulseview-glib-2.68.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;format_time_minutes_test is failing
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'remove-empty-doc-directory
            (lambda _
              (with-directory-excursion (string-append #$output "/share")
                ;; Use RMDIR to never risk silently deleting files.
                (rmdir "doc/pulseview")
                (rmdir "doc")))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list boost
           glib
           glibmm
           libsigrok
           libsigrokdecode
           qtbase-5
           qtsvg-5))
    (home-page "https://www.sigrok.org/wiki/PulseView")
    (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok")
    (description "PulseView is a Qt based logic analyzer, oscilloscope and MSO
GUI for sigrok.")
    (license license:gpl3+)))

(define-public osvvm
  (package
    (name "osvvm")
    (version "2025.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/osvvm/OsvvmLibraries/")
              (commit version)
              ;; OsvvmLibraries repository gathers all osvvm libraries as
              ;; submodules.
              (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08mfh7pyrb26mp8wx3xjns79slb3yf1c78nf8y1awvxc1p8q1wq4"))))
    (outputs
     '("out" "common" "scripts" "uart" "axi4"))
    (properties
     `((output-synopsis "out" "Verification Utility Library")
       (output-synopsis "common" "Common library")
       (output-synopsis "scripts" "Simulator script library")
       (output-synopsis "uart" "UART Verification Component Library")
       (output-synopsis "axi4" "AXI4 Verification Component Library")))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("osvvm" "share/osvvm/osvvm/"
           #:include ("vhd" "pro" "md")
           #:output "out")
          ("Common" "share/osvvm/Common/"
           #:include ("vhd" "pro" "md")
           #:output "common")
          ("Scripts" "share/osvvm/Scripts/"
           #:include ("tcl" "md")
           #:output "scripts")
          ("UART" "share/osvvm/UART"
           #:include ("vhd" "pro" "md")
           #:exclude-regexp ("GHDL_Debug")
           #:output "uart")
          ("AXI4" "share/osvvm/AXI4"
           #:include ("vhd" "pro" "md")
           #:output "axi4"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'fix-scripts
            (lambda _
              ;; Default conflicts with read-only /gnu/store.
              (substitute* "osvvm/OsvvmVhdlSettings.pro"
                (("\\[FindOsvvmSettingsDirectory\\]")
                 " \"\" ")))))))
    (native-search-paths
     (list (search-path-specification
             (variable "OSVVM")
             (separator #f)
             (files (list "share/osvvm")))))
    (home-page "https://osvvm.github.io/Overview/Osvvm1About.html/")
    (synopsis "The OSVVM VHDL Verification Libraries and Scripts")
    (description "OSVVM is a verification methodology that defines a VHDL
verification framework, verification utility library, verification component
library, scripting API, and co-simulation capability for FPGA or ASIC
verification.")
    (license license:asl2.0)))

(define-public python-cocotb
  (package
    (name "python-cocotb")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cocotb/cocotb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b9wc14df11wkwa20wkij4iip07841qsr0yir9g7dww069rj36q6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" "not test_toplevel_library")));requires questasim simulator
    (native-inputs
     (list iverilog
           nvc
           python-pytest
           python-setuptools-next))
    (propagated-inputs
     (list python-find-libpython))
    (home-page "https://github.com/cocotb/cocotb")
    (synopsis "Library for writing HDL test benches in Python")
    (description
     "Coroutine based cosimulation test bench environment for verifying VHDL
and Verilog RTL using Python.")
    (license license:bsd-3)))

(define-public python-cocotb-bus
  ;; XXX: The latest tagged release (2.6.1) was placed on <2023-07-01>, switch
  ;; to tag when the fresh release is available.
  (let ((commit "8269cbdacdc26e676eace4e19fc753c96ac9a059")
        (revision "0"))
    (package
      (name "python-cocotb-bus")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cocotb/cocotb-bus/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12762rdg630dq5qyvnv1g9kc36g0997nx8c5qndl34v6s9fc2152"))))
      (build-system pyproject-build-system)
      ;; TODO: Build documentation from <docs>.
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "make" "-k" "-C" "tests")
                  (invoke "make" "-k" "-C" "examples")))))))
      (native-inputs
       (list iverilog
             nvc
             python-pytest
             python-setuptools
             python-wheel))
      (propagated-inputs
       (list python-cocotb
             python-packaging
             python-scapy))
      (home-page "https://github.com/cocotb/cocotb-bus/")
      (synopsis "Cocotb reusable tools")
      (description "@code{Cocotb-bus} provides a set of utilities, test benches
and reusable bus interfaces to be used with @code{cocotb}.")
      (license license:bsd-3))))

(define-public python-edalize
  (package
    (name "python-edalize")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olofk/edalize/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03mkzkmi96jkrpgcnawixvy832p3b8li8lrirdjhfp9dmp7d5kg5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; XXX: Tests failing with assertion not equal, find out
                    ;; why.
                    (list "not test_gatemate"
                          "test_vcs_tool_options"
                          "test_vcs_no_tool_options"
                          "test_vcs_minimal"
                          "test_xcelium")
                    " and not "))))
    (native-inputs
     (list python-pytest python-setuptools-next))
    (propagated-inputs
     (list python-jinja2))
    (home-page "https://github.com/olofk/edalize/")
    (synopsis "Python Library for interacting with EDA tools")
    (description
     "This package implements a functionality to create project files for
supported tools and run them in batch or GUI mode.  All EDA tools such as
Icarus, Yosys, ModelSim, Vivado, Verilator, GHDL, Quartus etc get input HDL
files (Verilog and VHDL) and some tool-specific files (constraint files,memory
initialization files, IP description files etc).  Together with the files,
perhaps a couple of Verilog `defines, some top-level parameters/generics or
some tool-specific options are set.")
    (license license:bsd-2)))

(define-public python-pydigitalwavetools
  (package
    (name "python-pydigitalwavetools")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nic30/pyDigitalWaveTools/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fll8anz3i1j1nngsij1psp8766kvdfpls655lbxn2ykypv3633m"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/Nic30/pyDigitalWaveTools/")
    (synopsis "Library to manipulate digital wave files")
    (description
     "Pydigitalwavetools is a Python library to parse, write and format digital
wave files in @acronym{VCD, Value Change Dump} format, a standardized ASCII
format used to store simulation data from Verilog and other hardware description
languages.")
    (license license:expat)))

(define-public python-surf
  (package
    (name "python-surf")
    (version "2.57.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slaclab/surf/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ncb34mdxaw0m6cnk7kvl7mkhwa6hpcxkc2lgarwcmmnfydr8kg3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-deps
            (lambda _
              (invoke "git" "init") ;expects a git repo
              ;; fix version
              (substitute* "setup.py"
                (("rawVer .*")
                 (string-append "rawVer = \"v"
                                #$version "\""))))))))
    (native-inputs (list python-setuptools python-wheel python-gitpython
                         git-minimal/pinned))
    (home-page "https://slaclab.github.io/surf/")
    (synopsis "SLAC Ultimate RTL Framework")
    (description
     "Surf is a python library with support functions for VHDL gateware
digital design.  It provides implementation modules compatible with FPGA and ASIC
design.")
    (license (license:non-copyleft "file://LICENSE.txt"
                                   "See LICENSE.txt in the distribution."))))

(define-public python-vsg
  (package
    (name "python-vsg")
    (version "3.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeremiah-c-leary/vhdl-style-guide/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sryf1wv4r5maxj4di5rpsmzcxins3gq8aksv7cpw6ywvdk1nj5l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Tests are expensive and may introduce race condition on systems with
      ;; high (more than 16) threads count; limit parallel jobs to 8x.
      #~(list
         "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'pathch-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--cov=.*") "")
                ((".*--self-contained-html.*") "")
                ((".*-n.*auto.*") "")))))))
    (native-inputs
     (list python-pytest-cov
           python-pytest-html
           python-pytest-xdist
           python-setuptools-next))
    (propagated-inputs
     (list python-pyyaml))
    (home-page "https://github.com/jeremiah-c-leary/vhdl-style-guide/")
    (synopsis "Coding style enforcement for VHDL")
    (description
     "VSG lets you define a VHDL coding style and provides a command-line tool
to enforce it.")
    (license license:gpl3+)))

(define-public qucsator-rf
  (package
    (name "qucsator-rf")
    (version "1.0.7")                   ;required by qucs-s, keep in sync
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ra3xdh/qucsator_rf/")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qyih418r0jcrpk1ja4p7v9v5iqvri8iszg7s3vaf1d2agwblzb4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'run-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Qucs-test is a collection of python scripts and data test
                ;; cases. Its purpose is to test Qucs (GUI) and Qucsator;
                ;; tests are under `testsuite` directory.
                (copy-recursively
                 #$(origin
                     (method git-fetch)
                     (uri
                      ;; Using latest revision; refer to
                      ;; .github/workflows/cmake.yml to keep up to date.
                      (git-reference
                        (url "https://github.com/ra3xdh/qucs-test/")
                        (commit "ce69e05ceecab910175e6ea36b6e021a6d279947")))
                     (sha256
                      (base32
                       (string-append "1r3hx43wvd0s11mzsvj1chylzv"
                                      "0lk9qhaw7205j9x316ly03bl08"))))
                 "qucs-test")
                (with-directory-excursion "qucs-test"
                  (invoke "python3" "run.py" "--qucsator"
                          (format #f "--prefix=~a/bin" #$output)
                          "--exclude=skip.txt"))))))
      #:configure-flags
      #~(list (format #f "-DBISON_DIR=~a/bin"
                      #$(this-package-native-input "bison"))
              (format #f "-DADMSXML_DIR=~a/bin"
                      #$(this-package-native-input "adms")))))
    (native-inputs
     (list adms bison dos2unix flex gperf python python-looseversion
           python-numpy python-matplotlib))
    (synopsis "RF and microwave circuits simulator")
    (description
     "@code{Qucsator-rf} is a command line driven circuit simulator targeted
for RF and microwave circuits.  It takes a network list in a certain format as
input and outputs an XML dataset.")
    (home-page "https://ra3xdh.github.io//")
    (license license:gpl2+)))

(define-public qucs-s
  (package
    (name "qucs-s")
    (version "25.2.0")                  ;update qucsator-rf accordingly
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ra3xdh/qucs_s")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0328irynm8vy4xjdip5286fd8nag1zdp0p6rcbhdhp4fca6wp5ak"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase                   ;for Qt 6
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-default-settings
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "qucs/settings.cpp"
                (("\"ngspice\"")
                 (format #f "~s" (search-input-file inputs "bin/ngspice")))
                (("\"octave\"")
                 (format #f "~s" (search-input-file inputs "bin/octave"))))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/qucs-s")
                `("PATH" ":" prefix
                  (,(string-append #$(this-package-input "ngspice") "/bin")
                   ,(string-append
                     #$(this-package-input "qucsator-rf") "/bin")))))))))
    (native-inputs (list qttools))
    (inputs
     ;; TODO Add xyce-serial to the list.
     (list bash-minimal octave qtbase qtcharts qtsvg qtwayland qucsator-rf ngspice))
    (synopsis "GUI for different circuit simulation kernels")
    (description
     "@acronym{Qucs-S, Quite universal circuit simulator with SPICE} provides
a fancy graphical user interface for a number of popular circuit simulation
engines.  The package contains libraries for schematic capture, visualization
and components.  The following simulation kernels are supported:
@itemize
@item Ngspice (recommended)
@item Xyce
@item SpiceOpus
@item Qucsator (non-SPICE)
@end itemize\n")
    (home-page "https://ra3xdh.github.io/")
    (license license:gpl2+)))

(define-public xschem
  (package
    (name "xschem")
    (version "3.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/StefanSchippers/xschem")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0g9qrzm2mjd7nfg8iyc5az2bs8n5gjv1mrjjdja5vn1yjia7pvy9"))))
    (native-inputs (list flex bison pkg-config))
    (inputs (list gawk
                  tcl
                  tk
                  libxpm
                  cairo
                  libxrender
                  libxcb)) ; Last 3 are optional, but good to have.
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "CC" #$(cc-for-target))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (synopsis "Hierarchical schematic editor")
    (description
     "Xschem is an X11 schematic editor written in C and focused on
hierarchical and parametric design.  It can generate VHDL, Verilog or Spice
netlists from the drawn schematic, allowing the simulation of the circuit.")
    (home-page "https://xschem.sourceforge.io/stefan/index.html")
    (license license:gpl2+)))

(define-public route-rnd
  (package
    (name "route-rnd")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/route-rnd/"
                           "releases/route-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "0fy3b48s72lpicyap3y6jr9fyvb2ri42jb0gqxk6s927a278bfhc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/route-rnd/")
    (synopsis "Automatic routing for electronics boards")
    (description
     "@code{Route-rnd} is a generic external autorouter for @acronym{PCB,
Printed Circuit Board} using tEDAx file format, part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public sch-rnd
  (package
    (name "sch-rnd")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/sch-rnd/"
                           "releases/sch-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "07a1ik0rpsa5cscg9l7i5rnipx76543s7cdnkg802747rral7yj5"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/sch-rnd/")
    (synopsis "Scriptable editor of schematics for electronics boards")
    (description
     "@code{Sch-rnd} is a standalone and workflow agnostic schematics capture
tool for @acronym{PCB, Printed Circuit Board}, part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public sigrok-cli
  (package
    (name "sigrok-cli")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-cli/sigrok-cli-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f0a2k8qdcin0pqiqq5ni4khzsnv61l21v1dfdjzayw96qzl9l3i"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libsigrok libsigrokdecode))
    (build-system gnu-build-system)
    (home-page "https://sigrok.org/wiki/Sigrok-cli")
    (synopsis "Command-line frontend for sigrok")
    (description "Sigrok-cli is a command-line frontend for sigrok.")
    (license license:gpl3+)))

(define-public sigrok-firmware-fx2lafw
  ;; The project's last formal release was in 2019.
  ;;
  ;; The changes since then allow it to build with the latest version of SDCC,
  ;; 4.3.0.
  (let ((commit "96b0b476522c3f93a47ff8f479ec08105ba6a2a5")
        (revision "1"))
    (package
      (name "sigrok-firmware-fx2lafw")
      (version (git-version "0.1.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/sigrok-firmware-fx2lafw")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n5nj2g2m5ih59591ny2drrv25zviqcwyx1cfdhy8ijl82yxjkmb"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f))              ; no test suite
      (native-inputs
       (list autoconf automake sdcc))
      (home-page "https://www.sigrok.org/wiki/Fx2lafw")
      (synopsis "Firmware for Cypress FX2 chips")
      (description "Fx2lafw is free firmware for Cypress FX2 chips which makes
them usable as simple logic analyzer and/or oscilloscope hardware.")
      (license license:gpl2+))))

(define-public symbiyosys
  (package
    (name "symbiyosys")
    (version "0.57")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YosysHQ/sby/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w95svb9vgvfqbbvqaq6jkia5jldbai9l7r8nrx93wcfrm82r36x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:modules `((guix build gnu-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:imported-modules `(,@%default-gnu-imported-modules
                           (guix build python-build-system))
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          ;; TODO: build docs, after furo-ys is packaged.
          ;; (add-after 'install 'build-info
          ;; (lambda _
          ;; (invoke "make" "-C" "docs" "info")))
          (add-before 'check 'git-init
            (lambda _
              (invoke "git" "init")))   ;check expects a git repo
          (add-after 'git-init 'patch-/usr/bin/env
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "sbysrc/sby_core.py"
                (("\"/usr/bin/env\", ")
                 ""))
              (substitute* "sbysrc/sby.py"
                (("/usr/bin/env python")
                 (search-input-file inputs "bin/python3")))))
          (add-after 'install 'python:wrap
            (assoc-ref python:%standard-phases 'wrap)))))
    (inputs (list abc-yosyshq
                  boolector
                  git-minimal/pinned
                  python
                  python-click
                  python-xmlschema
                  z3
                  yices
                  yosys))
    ;; TODO: see above build-info phase comment.
    ;; (native-inputs (list
    ;;                 python-sphinx python-sphinx-argparse texinfo))
    (home-page "https://github.com/YosysHQ/sby/")
    (synopsis "Formal hardware verification with yosys")
    (description
     "SimbyYosys is a front-end program for yosys-based formal hardware
verification flows.")
    (license license:isc)))

(define-public uhdm
  (package
    (name "uhdm")
    (version "1.86")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsalliance/UHDM/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nsy385frxz5v7i757h1x59xkl21asz3h2fk1nyvx37z8cj0kd3z"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; there is no configure stage, as for INSTALL.md
          (delete 'configure))
      #:test-target "test"
      #:make-flags
      #~(list
         (string-append
          "ADDITIONAL_CMAKE_OPTIONS=-DUHDM_USE_HOST_CAPNP=On"
          ;; " -DUHDM_WITH_PYTHON=On"      ;FIXME
          " -DUHDM_USE_HOST_GTEST=On")
         (string-append "PREFIX=" #$output))))
    (native-inputs
     (list cmake-minimal googletest pkg-config python-wrapper swig))
    (inputs
     (list capnproto openssl python-orderedmultidict zlib))
    (home-page "https://github.com/chipsalliance/UHDM/")
    (synopsis "Universal Hardware Data Model")
    (description
     "UHDM is a complete modeling of the IEEE SystemVerilog Object Model with
VPI Interface, Elaborator, Serialization, Visitor and Listener.")
    (license license:asl2.0)))

(define-public xoscope
  (package
    (name "xoscope")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xoscope/xoscope/"
                                  version "/xoscope-" version ".tar.gz"))
              (sha256
               (base32
                "0a5ycfc1qdmibvagc82r2mhv2i99m6pndy5i6ixas3j2297g6pgq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 pkg-config))
    (inputs
     (list alsa-lib comedilib fftw gtk+ gtkdatabox))
    (synopsis "Digital oscilloscope")
    (description "Xoscope is a digital oscilloscope that can acquire signals
from ALSA, ESD, and COMEDI sources.  This package currently does not include
support for ESD sources.")
    (home-page "https://xoscope.sourceforge.net/")
    (license license:gpl2+)))
