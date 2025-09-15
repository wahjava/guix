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

;; Thanks to Tommy[D] for the Gentoo package which showed how to split up contrib!

(define *freenet/contrib-commit* "0e7962a81777a7e750a0b4fe4b8ab538e9bea14b")
(define *freenet/contrib-sha256* "02nv99qbwzb0x0vz0vpwnsc5rz1jymbaw5cz0r2s0adipx1j7y1j")
(define *freenet/contrib-source* (origin (method git-fetch)
                                         (uri (git-reference
                                               (url "https://github.com/hyphanet/contrib")
                                               (commit *freenet/contrib-commit*)))
                                         (sha256
                                          (base32 *freenet/contrib-sha256*))))
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

(define-public java-lzma
  (let* ((version "21.07")
         (version-filename (fold-right string-append "" ;eg. 21.07 -> 2107 for use in filenames
                                       (string-split version #\.))))
    (package
      (name "java-lzma")
      (version version)
      (source (origin (method url-fetch)
                      (uri (string-append "https://www.7-zip.org/a/lzma"
                                          version-filename
                                          ".7z"))
                      (sha256
                       (base32 "0x8dvzd0vqijl9z4qadsnxx6w18jh2xl8n6fcahcha367kq8hf43"))))
      (build-system ant-build-system)
      (arguments
       `(#:jar-name "lzma.jar"
         #:source-dir "source/Java/SevenZip"
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'unzip
                      (lambda* (#:key inputs #:allow-other-keys)
                        (mkdir "source")
                        (with-directory-excursion "source/"
                          (let ((7z (string-append (assoc-ref inputs "p7zip") "/bin/7z"))
                                (archive
                                 (string-append "../lzma"
                                                ,version-filename
                                                ".7z")))

                            (system* 7z "x" archive)))))
                    (replace 'install (install-jars "build")))))
      (native-inputs (list p7zip))
      (home-page "https://www.7-zip.org")
      (description "Java JNI bindings for lzma.")
      (synopsis "Java JNI LZMA")
      (license license:public-domain))))

(define-public java-lzmajio
  (package
    (name "java-lzmajio")
    (version "0.95-r3")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/league/lzmajio")
                          (commit "bfea68ba7f061c2cbcfded2c8e9c3a3f91436b91")))
                    (sha256
                     (base32 "1mrhknrk200jyl92n84bcl3chq29mdv5ps1899f48ag0bbvx4xy5"))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:phases (modify-phases %standard-phases
                            (replace 'install (install-jars ".")))))
    (inputs (list java-lzma))
    (native-inputs (list git java-junit java-lzma))
    (home-page "https://github.com/league/lzmajio")
    (description "LZMA Streams in Java")
    (synopsis "Java LZMA Streams")
    (license license:lgpl2.1)))

(define-public java-onion-common
  (package
    (name "java-onion-common")
    (version "1.0.4-r4")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/hyphanet/contrib")
                          (commit *freenet/contrib-commit*)))
                    (sha256
                     (base32 *freenet/contrib-sha256*))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:build-target "jars"
                 #:phases (modify-phases %standard-phases
                            (add-before 'build 'set-env
                              (lambda _
                                (chdir "onion-common/")))
                            (replace 'install
                              (install-jars "lib")))))
    (native-inputs
     (list gcc-toolchain))
    (home-page "https://github.com/hyphanet/contrib")
    (description "Locally maintained onionnetworks tools")
    (synopsis "Onionnetworks tools")
    (license license:expat)))

(define-public java-onion-fec
  (package
    (name "java-onion-fec")
    (version "1.0.4-r4")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/hyphanet/contrib")
                          (commit *freenet/contrib-commit*)))
                    (sha256
                     (base32 *freenet/contrib-sha256*))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:build-target "jars"
                 #:phases (modify-phases %standard-phases
                            (add-before 'build 'set-env
                              (lambda _
                                (chdir "onion-fec/")))
                            (replace 'install
                              (install-jars "lib")))))
    (native-inputs
     (list java-onion-common
           java-log4j-1.2-api))
    (inputs (list java-onion-common
                  onion-fec))
    (home-page "https://github.com/hyphanet/contrib")
    (description "Java support for FEC implementation by onionnetworks")
    (synopsis "Java Onnionnetworks FEC")
    (license license:expat)))

(define-public onion-fec                ;FIXME patch
  (package
    (name "onion-fec")
    (version "1.0.4-r4")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/hyphanet/contrib")
                          (commit *freenet/contrib-commit*)))
                    (sha256
                     (base32 *freenet/contrib-sha256*))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f
                 #:phases (modify-phases %standard-phases
                            (delete 'configure)
                            (add-before 'build 'set-env
                              (lambda _
                                (chdir "onion-fec/src/csrc")
                                (setenv "CC" (which "gcc"))
                                (setenv "JAVA_HOME" (assoc-ref %build-inputs "openjdk"))))
                            (replace 'install
                              (lambda _
                                (let* ((lib (string-append (assoc-ref %outputs "out") "/lib")))
                                  (install-file "libfec8.so" lib)
                                  (install-file "libfec16.so" lib)))))))
    (native-inputs
     (list `(,openjdk14 ,"jdk")))
    (home-page "https://github.com/hyphanet/contrib")
    (description "FEC implementation by onionnetworks")
    (synopsis "Onionnetworks FEC")
    (license license:expat)))

(define-public java-unbescape
  (package
    (name "java-unbescape")
    (version "unbescape-1.1.6.RELEASE")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/unbescape/unbescape")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03yfm9gwfgynqkyk8320sf4iy3pnqzm7zmpakk55saqk6lzk8xdi"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (mkdir-p "target/classes/META-INF")
                   ;; MANIFEST.MF copied from a build from git
                   (with-output-to-file "target/classes/META-INF/MANIFEST.MF"
                          (lambda _
                            (format #t "Manifest-Version: 1.0
Automatic-Module-Name: unbescape
Bnd-LastModified: 1706116789103
Build-Jdk: 9-internal
Built-By: guix
Bundle-Description: Advanced yet easy-to-use escape/unescape library for
  Java
Bundle-DocURL: http://www.unbescape.org
Bundle-License: http://www.apache.org/licenses/LICENSE-2.0.txt
Bundle-ManifestVersion: 2
Bundle-Name: unbescape
Bundle-SymbolicName: org.unbescape
Bundle-Vendor: The UNBESCAPE team
Bundle-Version: 1.1.6.RELEASE
Created-By: Apache Maven Bundle Plugin
Export-Package: org.unbescape;version=\"1.1.6\",org.unbescape.uri;version=
 \"1.1.6\",org.unbescape.html;version=\"1.1.6\",org.unbescape.java;version=\"
 1.1.6\",org.unbescape.css;version=\"1.1.6\",org.unbescape.csv;version=\"1.1
 .6\",org.unbescape.json;version=\"1.1.6\",org.unbescape.properties;version
 =\"1.1.6\",org.unbescape.xml;version=\"1.1.6\",org.unbescape.javascript;ver
 sion=\"1.1.6\"
Implementation-Title: unbescape
Implementation-URL: http://www.unbescape.org
Implementation-Vendor: The UNBESCAPE team
Implementation-Vendor-Id: org.unbescape
Implementation-Version: 1.1.6.RELEASE
Require-Capability: osgi.ee;filter:=\"(&(osgi.ee=JavaSE)(version=1.6))\"
Specification-Title: unbescape
Specification-Vendor: The UNBESCAPE team
Specification-Version: 1.1.6.RELEASE
Tool: Bnd-3.5.0.201709291849
X-Compile-Source-JDK: 6
X-Compile-Target-JDK: 6")))))))
    (build-system maven-build-system)
    (arguments
     `(#:exclude
       (("org.apache.maven.plugins" .
         ("maven-source-plugin" "maven-javadoc-plugin" "maven-gpg-plugin"
          "maven-release-plugin" "maven-assembly-plugin" "maven-surefire-plugin"
          "maven-site-plugin"))
        ("com.mycila.maven-license-plugin" . ("maven-license-plugin"))
        ("org.apache.felix" . ("maven-bundle-plugin")))
       #:maven-plugins
       (("maven-enforcer-plugin" ,maven-enforcer-plugin)
        ,@(default-maven-plugins))))
    (propagated-inputs
     (list java-jopt-simple-4
           java-commons-math3))
    (native-inputs
     (list java-junit
           java-hamcrest-core
           java-commons-lang3
           maven-model-builder
           maven-resolver-provider
           maven-resolver-impl
           java-guice))
    (home-page "https://github.com/unbescape/unbescape")
    (synopsis "Java escaping library")
    (description "Unbescape is a Java library aimed at performing fully-featured and high-performance escape and unescape operations for , HTML (HTML5 and HTML 4), XML (XML 1.0 and XML 1.1), JavaScript, JSON, URI/URL, CSS, CSV (Comma-Separated Values), Java literals, and Java .properties files")
    (license license:asl2.0)))
