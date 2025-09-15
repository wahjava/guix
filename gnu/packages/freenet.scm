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

(define-public freenet-ext
  (package
    (name "freenet-ext")
    (version "50")
    (source *freenet/contrib-source*)
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:source-dir "freenet-ext/src"
                 #:jar-name "freenet-ext.jar"
                 #:phases (modify-phases %standard-phases
                            (replace 'install
                              (install-jars ".")))))
    (home-page "https://github.com/hyphanet/contrib")
    (description "freenet-ext provides external libraries needed for Freenet.")
    (synopsis "External libraries for hyphanet")
    (license license:gpl2+)))

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


(define-public freenet-db4o
  (package
    (name "freenet-db4o")
    (version "7.4")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/xor-freenet/db4o-7.4")
                          (commit "831aecf5f88a8f6716707c61249d55d627246fac")))
                    (sha256
                     (base32 "1njzric4c6c9kgvrx49ssr8l46jwsaxg1j7g2gh8hy3v4dal0sw7"))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:phases (modify-phases %standard-phases
                            (replace 'install
                              (install-jars ".")))))
    (home-page "https://web.archive.org/web/20140517213411/http://www.db4o.com/")
    (description "freenet-db4o: Database for Objects as needed by Freenet.")
    (synopsis "Java object database v7")
    (license license:gpl2)))

(define-public freenet-native-big-integer
  (package
   (name "freenet-native-big-integer")
   (version "contrib")
   (source *freenet/contrib-source*)
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:phases (modify-phases %standard-phases
                 (delete 'configure)
                 (replace 'build
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((gcc (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
                           (gmp (string-append (assoc-ref inputs "gmp") "/lib/"))
                           (java-incl. (string-append (assoc-ref inputs "openjdk") "/include/"))
                           (source "jbigi/src/jbigi.c")
                           (compile-flags "-fPIC -Wall")
                           (link-flags "-shared -Wl,-soname,libjbigi.so"))
                       (chdir "NativeBigInteger")
                       (system* gcc
                                "-c" compile-flags
                                (string-append "-I" java-incl.
                                               " -I" java-incl. "/linux/"
                                               " -I" "jbigi/include/")
                                source
                                link-flags
                                (string-append " -L" gmp
                                               " -lgmp")))))
                 (replace 'install
                   (lambda* (#:key outputs system #:allow-other-keys)
                     (let* ((sys (string-split system #\-))
                            (os-arch (string-append (cadr sys) "-" (car sys))))
                       (map (lambda (obj-file)
                              (install-file obj-file
                                            (string-append (assoc-ref outputs "out")
                                                           "/lib/")))
                            (find-files "." os-arch))))))))
   (inputs (list gmp))
   (native-inputs (list `(,openjdk14 "jdk")))
   (home-page "https://github.com/hyphanet/contrib")
   (description "native big integer implementation adapted from i2p.")
   (synopsis "Native big integer")
   (license license:gpl2+)))   ; code from i2p is BSD, commits by Freenet devs are usually GPL2+, so GPL2+ is safe

(define-public freenet-native-thread    ;FIXME Patch and arch.
  (package
    (name "freenet-native-thread")
    (version "contrib")
    (source *freenet/contrib-source*)
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:modules ((ice-9 regex) ,@%default-gnu-imported-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((java-home (assoc-ref inputs "icedtea")))
               (setenv "JAVA_HOME" java-home)
               (chdir "NativeThread/"))))
         (replace 'build
           (lambda _ (system* "make" "all")))
         (replace 'install
           (lambda* (#:key outputs system #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib/"))
                   (obj-dir "lib/freenet/support/io/")
                   (lib-file (if ,(target-x86-64?)
                                 "/libNativeThread-amd64.so"
                                 "/libNativeThread-i386.so")))
               (install-file (string-append obj-dir lib-file) lib)))))))
    (native-inputs (list `(,icedtea "jdk")))
    (home-page "https://github.com/hyphanet/contrib" )
    (description "Java tooling for native thread specifics")
    (synopsis "Java native thread tools")
    (license license:gpl2+)))

(define-public freenet-jcpuid
  (package
    (name "freenet-jcpuid")
    (version "contrib")
    (source *freenet/contrib-source*)
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((g++ (string-append (assoc-ref inputs "gcc") "/bin/g++"))
                   (openjdk (string-append (assoc-ref inputs "openjdk") "/include/"))
                   (source "src/jcpuid.cpp")
                   (flags "-shared -static -static-libgcc -fPIC")
                   (obj "libjcpuid-linux.so"))
               (chdir "jcpuid")
               (system* g++ flags
                        " -Iinclude/"
                        (string-append " -I" openjdk)
                        (string-append " -I" openjdk "/linux/")
                        source))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib/")))
               (install-file "lib/freenet/support/CPUInformation/libjcpuid-x86-linux.so" lib)))))))
    (native-inputs (list `(,openjdk14 "jdk")))
    (home-page "https://github.com/hyphanet/contrib")
    (description "native CPU info tools for Java")
    (synopsis "Java CPU info")
    (license license:gpl2+)))

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

(define-public java-freenet-mantissa
  (package
    (name "java-freenet-mantissa")
    (version "0_pre23765")
    (source (origin (method url-fetch)
                    (uri "https://distfiles.gentoo.org/distfiles/mersennetwister-0_pre23765.tar.bz2")
                    (sha256
                     (base32 "1szr4w04abasgcsswzaawha4fmj8h9327jy8yzaybr5a99hv07vq"))))
    (build-system ant-build-system)
    (arguments `(#:tests? #f
                 #:jar-name "mantissa.jar"
                 #:source-dir "org"
                 #:phases (modify-phases %standard-phases
                            (add-before 'build 'set-pwd
                              (lambda _
                                (chdir "../")))
                            (replace 'install (install-jars ".")))))
    (home-page "http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html")
    (description "Modified Mersenne Twister java implementation for Freenet")
    (synopsis "Java mersenne twister random number generator")
    (license license:bsd-0)))

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

(define-public java-pebble
  (package
    (name "java-pebble")
    (version "3.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PebbleTemplates/pebble")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s3xpjbn6dhmw5mszs72zpnzwapk894vvc1682blij7rp80x201r"))))
    (build-system maven-build-system)
    (arguments
     `(#:tests? #f
       #:exclude ;; packages to remove from pom
       (("org.apache.maven.plugins" .
         ("maven-source-plugin" "maven-archetype-plugin" "maven-shade-plugin"
          "maven-site-plugin" "maven-javadoc-plugin" "maven-eclipse-plugin"
          "maven-gpg-plugin" "maven-release-plugin" "maven-assembly-plugin"))
        ("com.mycila.maven-license-plugin" . ("maven-license-plugin"))
        ("com.github.ben-manes.caffeine" . ("caffeine"))
        ("ch.qos.logback" . ("logback-classic"))
        ("org.junit-jupiter". ("junit-jupiter"))
        ("org.assertj" . ("assertj-core"))
        ("org.apache.felix" . ("maven-bundle-plugin")))
       #:maven-plugins
       (("maven-enforcer-plugin" ,maven-enforcer-plugin)
        ,@(default-maven-plugins))
       #:phases
       (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "pebble") #t))
           (add-after 'chdir 'delete-parts-with-unmet-dependencies
             (lambda _
               (use-modules (ice-9 rdelim) (ice-9 string-fun))
               (delete-file-recursively "src/test/")
               ; (delete-file-recursively "src/main/java/com/mitchellbosecke/pebble/loader/ServletLoader.java")
               (delete-file-recursively "src/main/java/com/mitchellbosecke/pebble/loader/Servlet5Loader.java")
               (delete-file-recursively "src/main/java/com/mitchellbosecke/pebble/cache/template/CaffeineTemplateCache.java")
                                        (delete-file-recursively "src/main/java/com/mitchellbosecke/pebble/cache/tag/CaffeineTagCache.java")
               ))
           (add-after 'chdir 'remove-plugins-in-toplevel
             (lambda _
               (use-modules (ice-9 rdelim) (ice-9 string-fun))
               (with-atomic-file-replacement "../pom.xml"
                 (lambda (in out) (let ((str (read-delimited "" in)))
                                    (display
                                     (string-replace-substring
                                      (string-replace-substring str "
      <plugin>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>3.6.2</version>
        <configuration>
          <source>${java.version}</source>
          <target>${java.version}</target>
        </configuration>
      </plugin>
      <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-jar-plugin</artifactId>
          <version>3.1.0</version>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-plugin</artifactId>
        <version>2.22.1</version>
        <configuration>
          <argLine>-Dfile.encoding=${project.build.sourceEncoding}</argLine>
        </configuration>
      </plugin>
      <plugin>
        <groupId>org.sonatype.plugins</groupId>
        <artifactId>nexus-staging-maven-plugin</artifactId>
        <version>1.6.8</version>
        <extensions>true</extensions>
        <configuration>
          <serverId>ossrh</serverId>
          <nexusUrl>https://oss.sonatype.org/</nexusUrl>
          <autoReleaseAfterClose>true</autoReleaseAfterClose>
        </configuration>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-release-plugin</artifactId>
        <version>2.5.3</version>
        <configuration>
          <tagNameFormat>v@{project.version}</tagNameFormat>
          <autoVersionSubmodules>true</autoVersionSubmodules>
          <useReleaseProfile>false</useReleaseProfile>
          <releaseProfiles>release</releaseProfiles>
          <goals>deploy</goals>
        </configuration>
      </plugin>" "
      <plugin>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>3.8.1</version>
        <configuration>
          <source>${java.version}</source>
          <target>${java.version}</target>
        </configuration>
      </plugin>
")  "          <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-source-plugin</artifactId>
            <version>2.2.1</version>
            <executions>
              <execution>
                <id>attach-sources</id>
                <goals>
                  <goal>jar</goal>
                </goals>
              </execution>
            </executions>
          </plugin>
          <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-javadoc-plugin</artifactId>
            <version>2.9.1</version>
            <executions>
              <execution>
                <id>attach-javadocs</id>
                <goals>
                  <goal>jar</goal>
                </goals>
              </execution>
            </executions>
          </plugin>
          <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-gpg-plugin</artifactId>
            <version>1.5</version>
            <executions>
              <execution>
                <id>sign-artifacts</id>
                <phase>verify</phase>
                <goals>
                  <goal>sign</goal>
                </goals>
              </execution>
            </executions>
          </plugin>
" "") out))))))
           (add-after 'chdir 'install-maven-dependencies
             (lambda _ #f))
           (add-after 'chdir 'remove-testing-and-optional-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (use-modules (ice-9 rdelim) (ice-9 string-fun))
               (with-atomic-file-replacement "pom.xml"
                 (lambda (in out) (let ((str (read-delimited "" in)))
                                    (display (string-replace-substring
                                              (string-replace-substring str "<!-- optional dependencies -->
    <dependency>
      <groupId>com.github.ben-manes.caffeine</groupId>
      <artifactId>caffeine</artifactId>
      <version>${caffeine.version}</version>
      <optional>true</optional>
    </dependency>
    <dependency>
      <groupId>javax.servlet</groupId>
      <artifactId>servlet-api</artifactId>
      <version>${servlet-api.version}</version>
      <optional>true</optional>
    </dependency>
    <dependency>
      <artifactId>jakarta.servlet-api</artifactId>
      <groupId>jakarta.servlet</groupId>
      <version>5.0.0</version>
      <optional>true</optional>
    </dependency>

    <!-- testing dependencies -->
    <dependency>
      <groupId>ch.qos.logback</groupId>
      <artifactId>logback-classic</artifactId>
      <version>${logback-classic.version}</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>org.junit.jupiter</groupId>
      <artifactId>junit-jupiter</artifactId>
      <version>${junit-jupiter.version}</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>org.assertj</groupId>
      <artifactId>assertj-core</artifactId>
      <version>${assertj.version}</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>commons-io</groupId>
      <artifactId>commons-io</artifactId>
      <version>${commons-io.version}</version>
      <scope>test</scope>
    </dependency>" "")
 "<plugin><version>3.2.0</version>
        <artifactId>maven-jar-plugin</artifactId>
        <configuration>
          <archive>
            <manifestEntries>
              <Automatic-Module-Name>io.pebbletemplates</Automatic-Module-Name>
            </manifestEntries>
          </archive>
        </configuration>
      </plugin>
" "") out))))))
           (add-after 'fix-pom-files 'add-servlet-system-path
             (lambda* (#:key inputs #:allow-other-keys)
               (use-modules (ice-9 rdelim) (ice-9 string-fun))
               (with-atomic-file-replacement "pom.xml"
                 (lambda (in out) (let ((str (read-delimited "" in)))
                                    (display (string-replace-substring str "<dependencies>"
                                                                       (string-append "<dependencies>
    <dependency>
      <groupId>javax.servlet</groupId>
      <artifactId>servlet-api</artifactId>
      <version>${servlet-api.version}</version>
      <optional>false</optional>
      <scope>system</scope>
      <systemPath>" (search-input-file inputs "share/java/javax-servletapi.jar")
      "</systemPath>
    </dependency>
"
                                  )) out))))))
             )))
    (propagated-inputs
     (list java-commons-io))
    (native-inputs
     (list java-junit
           java-assertj
           java-commons-io
           java-slf4j-api
           java-unbescape
           java-logback-classic
           java-commons-lang3
           java-javaee-servletapi
           maven-compiler-plugin
           maven-model-builder
           maven-resolver-provider
           maven-resolver-impl))
    (home-page "https://github.com/unbescape/unbescape")
    (synopsis "Java templating engine")
    (description "Java templating engine inspired by Twig and similar to the Python Jinja Template Engine syntax.")
    (license license:asl2.0)))
