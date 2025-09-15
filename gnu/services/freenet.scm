(define-module (gnu services freenet)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freenet)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages certs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (freenet-configuration
            freenet-configuration?
            freenet-service-type))

(define (freenet-ini-content output-bandwidth-limit-bytes/s store-size-bytes opennet-enabled update-uri seed address-override)
  (format #f "fproxy.hasCompletedWizard=true
fproxy.bindTo=127.0.0.1,0:0:0:0:0:0:0:1
fproxy.allowedHostsFullAccess=127.0.0.1,0:0:0:0:0:0:0:1
pluginmanager.loadplugin=UPnP2;KeepAlive;Sharesite
pluginmanager.enabled=true
node.slashdotCacheSize=27648k
node.minDiskFreeShortTerm=536870912
node.downloadsDir=./downloads
node.uploadAllowedDirs=all
node.minDiskFreeLongTerm=1073741824
node.outputBandwidthLimit=~a
node.storeSize=~a
node.assumeNATed=true
node.bindTo=0.0.0.0
node.clientCacheSize=33554432
node.storeType=salt-hash
node.clientCacheType=ram
node.updater.updateSeednodes=true
node.updater.updateInstallers=true
node.opennet.enabled=~a
node.updater.URI=~a
node.opennet.assumeNATed=true
node.opennet.acceptSeedConnections=~a
node.opennet.bindTo=0.0.0.0
~aEnd
" output-bandwidth-limit-bytes/s store-size-bytes opennet-enabled update-uri seed address-override))

(define-record-type* <freenet-configuration>
  freenet-configuration
  make-freenet-configuration freenet-configuration?
  this-freenet-configuration
  (freenet-package freenet-configuration-package (default fred))
  (user freenet-configuration-user (default #f))
  (group freenet-configuration-group (default "users"))
  (working-dir freenet-configuration-working-dir (default "/var/lib/hyphanet/"))
  (max-memory freenet-configuration-max-memory (default "2048M"))
  (store-size-mb freenet-configuration-store-size (default 5000))
  (output-bandwidth-kBps freenet-configuration-output-bandwidth-bytes/s (default 200))
  (opennet-enabled freenet-configuration-opennet-enabled (default #t))
  (update-uri freenet-configuration-update-uri (default "USK@vCKGjQtKuticcaZ-dwOgmkYPVLj~N1dm9mb3j3Smg4Y,-wz5IYtd7PlhI2Kx4cAwpUu13fW~XBglPyOn8wABn60,AQACAAE/jar/1503"))
  (address-override freenet-configuration-address-override (default #f))
  (seed freenet-configuration-seed (default #f))
  (freenet.ini freenet-configuration-freenet.ini (default #f)))

(define freenet-service
  (match-lambda
    (($ <freenet-configuration> freenet-package user group work-dir max-memory store-size-mb output-bandwidth-kBps opennet-enabled update-uri address-override seed freenet.ini)
     (list
      (let* ((user (or user "root"))
             (service-name "freenet-hyphanet")
             (working-dir work-dir)
             (freenet-input (lambda (input)
                              (car (assoc-ref (package-inputs freenet-package) input))))
             (ini-content (freenet-ini-content
                           (inexact->exact (ceiling (* 1024 output-bandwidth-kBps)))
                           (inexact->exact (ceiling (* 1024 1024 store-size-mb)))
                           (if opennet-enabled "true" "false")
                           update-uri
                           (if seed "true" "false")
                           (if address-override (format #f "node.ipAddressOverride=~a\n" address-override) ""))))
        (shepherd-service
         (provision (list (string->symbol service-name)))
         (documentation "Run a Hyphanet node")
         (requirement '(networking))
         (start
          #~(begin
              (unless (directory-exists? #$working-dir)
                (mkdir-p #$working-dir)
                (chmod #$working-dir #o755)
                (chown #$working-dir (passwd:uid (getpw #$user)) (group:gid (getgr #$group))))
              (chdir #$working-dir)
              (let ((seeds (string-append #$working-dir "seednodes.fref")))
                (unless (file-exists? seeds)
                  (copy-file (string-append #$(freenet-input "freenet-seedrefs") "/var/seednodes.fref") seeds)))
              (let ((ini (string-append #$working-dir "freenet.ini")))
                (unless (file-exists? ini)
                  (with-output-to-file ini
                    (lambda _
                      (display (or #$freenet.ini #$ini-content))))))
              (let ((classpath (string-append #$(freenet-input "freenet-ext") "/share/java/freenet-ext.jar"
                                              ":" #$(freenet-input "java-bouncycastle") "/share/java/bcprov-jdk15on-167.jar"
                                              ":" #$(freenet-input "java-freenet-mantissa") "/share/java/mersennetwister.jar"
                                              ":" #$(freenet-input "java-lzma") "/share/java/lzma.jar"
                                              ":" #$(freenet-input "java-lzmajio") "/share/java/lzmajio.jar"
                                              ":" (let ((cc #$(freenet-input "java-commons-compress"))
                                                        (cc-ver #$(package-version (freenet-input "java-commons-compress"))))
                                                    (string-append cc "/lib/m2/org/apache/commons/commons-compress/"
                                                                   cc-ver "/" "commons-compress-" cc-ver ".jar"))
                                              ":" #$(freenet-input "java-native-access") "/share/java/jna.jar"
                                              ":" #$(freenet-input "java-native-access-platform") "/share/java/jna-platform.jar"
                                              ":" #$(freenet-input "java-bdb-je") "/share/java/je.jar"
                                              ":" #$(freenet-input "java-onion-fec") "/share/java/onion-fec.jar"
                                              ":" #$(freenet-input "java-onion-common") "/share/java/onion-common.jar"
                                              ":" #$(freenet-input "java-service-wrapper") "/share/java/wrapper.jar"
                                              ":" #$(freenet-input "java-jbitcollider-core") "/share/java/jbitcollider-core-0.8.jar"
                                              ":" #$(freenet-input "java-pebble") "/lib/m2/io/pebbletemplates/pebble/3.1.6-SNAPSHOT/pebble-3.1.6-SNAPSHOT.jar"
                                              ":" #$(freenet-input "java-javaee-servletapi") "/share/java/javax-servletapi.jar"
                                              ":" #$(freenet-input "java-unbescape") "/lib/m2/org/unbescape/unbescape/1.1.6.RELEASE/unbescape-1.1.6.RELEASE.jar"
                                              ":" #$(freenet-input "java-commons-lang3") "/lib/m2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
                                              ":" #$(freenet-input "java-logback-classic") "/share/java/logback-classic.jar"
                                              ":" #$(freenet-input "java-logback-core") "/share/java/logback.jar"
                                              ":" #$(freenet-input "java-commons-io") "/lib/m2/commons-io/commons-io/2.5/commons-io-2.5.jar"
                                              ":" #$(freenet-input "java-slf4j-api") "/lib/m2/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar"
                                              ":" #$(freenet-input "maven-slf4j-provider") "/lib/m2/org/apache/maven/maven-slf4j-provider/3.9.0/maven-slf4j-provider-3.9.0.jar"
                                              #$(package-version (freenet-input "java-jbitcollider-core")) ".jar"
                                              ":" #$(freenet-input "freenet-db4o") "/share/java/db4o.jar"
                                              ":" #$freenet-package "/share/java/freenet.jar"))) ;; freenet.jar last so the templates are found
                (format #t "freenet-hyphanet classpath: ~a" classpath)
                (make-forkexec-constructor
                 (list (string-append #$coreutils "/bin/nice")
                       (string-append #$(freenet-input "openjdk") "/bin/java")
                       "-Dfreenet.jce.use.NSS=true"
                       "-Djava.io.tmpdir=./tmp/"
                       "-Dnetworkaddress.cache.ttl=0"
                       "-Dnetworkaddress.cache.negative.ttl=0"
                       "--add-opens=java.base/java.lang=ALL-UNNAMED"
                       "--add-opens=java.base/java.util=ALL-UNNAMED"
                       "--add-opens=java.base/java.io=ALL-UNNAMED"
                       "-Xss512k"
                       "-cp"
                       classpath
                       (string-append "-Xmx" #$max-memory)
                       "freenet.node.NodeStarter")
                 #:user #$user
                 #:group #$group
                 #:log-file (string-append #$working-dir "fred.log")
                 #:directory #$working-dir
                 #:environment-variables
                 (list (string-append "PWD=" #$working-dir)
                       (string-append "SSL_CERT_DIR=" #$nss-certs "/etc/ssl/certs/")
                       (string-append "LD_LIBRARY_PATH="
                                      (string-join
                                       (list
                                        (string-append #$(freenet-input "onion-fec") "/lib/")
                                        (string-append #$(freenet-input "freenet-native-thread") "/lib/")
                                        (string-append #$(freenet-input "freenet-native-big-integer") "/lib/")
                                        (string-append #$(freenet-input "freenet-jcpuid") "/lib/")
                                        (string-append #$(freenet-input "java-service-wrapper") "/lib/")
                                        (unless (getenv "LD_LIBRARY_PATH") "")) ":"))
                       )))))

         (stop #~(make-kill-destructor))))))))


(define freenet-service-type
  (service-type (name 'freenet-hyphanet)
                (extensions (list (service-extension shepherd-root-service-type freenet-service)))
                (description "Service type for Freenet censorship resistant network.")
                (default-value (freenet-configuration))))
