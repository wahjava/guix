(define-module (gnu packages freenet)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system maven)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages image)
  #:use-module (gnu packages base)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages java-compression)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages java)
  #:use-module (gnu packages web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages check)
  #:export (fred))
(define-public java-bdb-je
  (package
    (name "java-bdb-je")
    (version "7.5.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.oracle.com/berkeley-db/je-"
                           version ".tar.gz"))
       (sha256
        (base32 "0k3grm58jm2iry7m548ag6nmrplxq39swkif1pf9fnpzdpk0a9gq"))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:phases (modify-phases %standard-phases
                            (replace 'install
                              (install-jars "build/lib/")))))
    (native-inputs (list inetutils))
    (home-page "https://www.oracle.com/database/technologies/related/berkeleydb.html")
    (description "Implementation of the Berkeley DB in pure Java.")
    (synopsis "Java Berkeley DB")
    (license license:asl2.0)))

(define-public java-jbitcollider-core
  (package
    (name "java-jbitcollider-core")
    (version "0.8")
    (source (origin (method url-fetch)
                    (uri
                     (string-append
                      "mirror://sourceforge/project/bitcollider/jBitcollider%20%28Java%29/"
                      version "/jBitcollider-" version ".zip"))
                    (sha256
                     (base32 "0i0hhrhz7pz11wl4wrk7mw2zbvap1zjzfzvrn6dnd74ikvp84r5k"))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:jar-name "jbitcollider-core-0.8.jar"
                 #:source-dir "plugins/org.bitpedia.collider.core/src"
                 #:phases (modify-phases %standard-phases
                            (replace 'install (install-jars "build/jar")))))
    (native-inputs (list unzip))
    (home-page "http://bitcollider.sourceforge.net/")
    (description "Bitcollider tools in Java")
    (synopsis "Java bitcollider tools")
    (license license:public-domain)))
