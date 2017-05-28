;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages animation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video))

(define-public etl
  (package
    (name "etl")
    (version "0.04.22")
    (source (origin
              (method url-fetch)
              ;; Keep this synchronized with the synfig release version.
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  "1.2.0/source/ETL-" version ".tar.gz"))
              (sha256
               (base32
                "0ii73nsd3xzkhz6w1rnxwphl637j9w82xiy6apa9vin2isdynnmc"))))
    (build-system gnu-build-system)
    (home-page "http://www.synfig.org")
    (synopsis "Extended C++ template library")
    (description
     "ETL is a class and template library designed to add new datatypes and
functions which combine well with the existing types and functions from the
C++ @dfn{Standard Template Library} (STL).")
    (license license:gpl3+)))

(define-public synfig
  (package
    (name "synfig")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  version "/source/synfig-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1gqx4gn4c73rqwhsgzx0a460gr9hadmi28csp75rx30qavqsj7k1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; The Boost library path is taken from the value of BOOST_LDFLAGS.
       (list (string-append "BOOST_LDFLAGS=-L"
                            (assoc-ref %build-inputs "boost")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adapt-to-libxml++-changes
          (lambda _
            (substitute* "configure"
              (("libxml\\+\\+-2\\.6") "libxml++-3.0"))
            (substitute* (append (find-files "src/modules/" "\\.cpp$")
                                 (find-files "src/synfig/" "\\.(cpp|h)$"))
              (("add_child\\(") "add_child_element(")
              (("get_child_text\\(") "get_first_child_text(")
              (("set_child_text\\(") "set_first_child_text(")
              (("remove_child\\(") "remove_node("))
            (substitute* "src/modules/mod_svg/svg_parser.cpp"
              (("xmlpp::Node::NodeList") "xmlpp::Node::const_NodeList"))
            #t)))))
    (inputs
     `(("boost" ,boost)
       ("ffmpeg" ,ffmpeg)
       ("libdv" ,libdv)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libmng" ,libmng)
       ("zlib" ,zlib)))
    ;; synfig.pc lists the following as required: Magick++ freetype2
    ;; fontconfig fftw OpenEXR ETL glibmm-2.4 giomm-2.4 libxml++-3.0 sigc++-2.0
    ;; cairo pango pangocairo mlt++
    (propagated-inputs
     `(("cairo" ,cairo)
       ("etl" ,etl)
       ("fftw" ,fftw)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glibmm" ,glibmm)
       ("imagemagick" ,imagemagick)
       ("libxml++" ,libxml++)
       ("libsigc++" ,libsigc++)
       ("mlt" ,mlt)
       ("openexr" ,openexr)
       ("pango" ,pango)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.synfig.org")
    (synopsis "Vector-based 2D animation renderer")
    (description
     "Synfig is a vector-based 2D animation package.  It is designed to be
capable of producing feature-film quality animation.  It eliminates the need
for tweening, preventing the need to hand-draw each frame.")
    (license license:gpl3+)))

(define-public synfigstudio
  (package
    (name "synfigstudio")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  version "/source/synfigstudio-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0fbckfbw8dzf0m2wv7vlmw492k1dqa3zf510z019d0as3zpnp6qm"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/synfigapp/pluginmanager.cpp"
                    (("xmlpp::Node\\* n =")    "const xmlpp::Node* n =")
                    (("xmlpp::Node::NodeList") "xmlpp::Node::const_NodeList"))
                  #t))
              (patches
               (search-patches "synfigstudio-fix-ui-with-gtk3.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtkmm" ,gtkmm)
       ("libsigc++" ,libsigc++)
       ("synfig" ,synfig)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (home-page "http://www.synfig.org")
    (synopsis "Vector-based 2D animation package (GUI)")
    (description
     "Synfig is a vector-based 2D animation package.  It is designed to
be capable of producing feature-film quality animation.  It eliminates the
need for tweening, preventing the need to hand-draw each frame.  This package
contains the graphical user interface for synfig.")
    (license license:gpl3+)))
