;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2017, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
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

(define-module (gnu packages certs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls))

(define-public certdata2pem
  (let ((revision "1")
        (commit "4c576f350f44186d439179f63d5be19f710a73f5"))
    (package
      (name "certdata2pem")
      (version "0.0.0")                   ;no version
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://raw.githubusercontent.com/sabotage-linux/sabotage/"
                      commit "/KEEP/certdata2pem.c"))
                (sha256
                 (base32
                  "1rywp29q4l1cs2baplkbcravxqs4kw2cys4yifhfznbc210pskq6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (add-before 'build 'fix-extension
                      (lambda _
                        (substitute* "certdata2pem.c"
                          (("\\.crt")
                           ".pem"))))
                    (replace 'build
                      (lambda _
                        (invoke ,(cc-for-target) "certdata2pem.c"
                                "-o" "certdata2pem")))
                    (delete 'check)     ;no test suite
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (install-file "certdata2pem"
                                        (string-append out "/bin"))))))))
      (home-page "https://github.com/sabotage-linux/")
      (synopsis "Utility to split TLS certificates data into multiple PEM files")
      (description "This is a C version of the certdata2pem Python utility
that was originally contributed to Debian.")
      (license license:isc))))

(define-public desec-certbot-hook
  (let ((commit "68da7abc0793602fd336962a7e2348b57c5d6fd6")
        (revision "0"))
    (package
      (name "desec-certbot-hook")
      (version
       (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/desec-io/desec-certbot-hook")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qjqk6i85b1y7fgzcx74r4gn2i4dkjza34hkzp6kyn9hrb8f2gv2"))))
      (build-system copy-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-script
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hook.sh"
                 ;; The hook-script look for '.dedynauth' file in $PWD.
                 ;; But users cannot create or edit files in store.
                 ;; So we patch the hook-script to look for '.dedynauth' file,
                 ;; in /etc/desec.
                 (("\\$\\(pwd\\)")
                  "/etc/desec")
                 ;; Make absolute reference to curl program.
                 (("curl")
                  (string-append (assoc-ref inputs "curl")
                                 "/bin/curl"))))))
         #:install-plan
         '(("." "etc/desec" #:include ("hook.sh")))))
      (inputs
       (list curl))
      (synopsis "Certbot DNS challenge automatization for deSEC")
      (description "The deSEC can be used to obtain certificates with certbot
DNS ownership verification.  With the help of this hook script, you can obtain
your Let's Encrypt certificate using certbot with authorization provided by the
DNS challenge mechanism, that is, you will not need a running web server or any
port forwarding to your local machine.")
      (home-page "https://desec.io")
      (license license:expat))))

(define-public le-certs
  (package
    (name "le-certs")
    (version "1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((root-rsa (assoc-ref %build-inputs "isrgrootx1.pem"))
               (root-ecdsa (assoc-ref %build-inputs "isrgrootx2.pem"))
               (intermediate-rsa (assoc-ref %build-inputs "letsencryptauthorityr3.pem"))
               (intermediate-ecdsa (assoc-ref %build-inputs "letsencryptauthoritye1.pem"))
               (backup-rsa (assoc-ref %build-inputs "letsencryptauthorityr4.pem"))
               (backup-ecdsa (assoc-ref %build-inputs "letsencryptauthoritye2.pem"))
               (out (string-append (assoc-ref %outputs "out") "/etc/ssl/certs"))
               (openssl (assoc-ref %build-inputs "openssl"))
               (perl (assoc-ref %build-inputs "perl")))
           (mkdir-p out)
           (for-each
             (lambda (cert)
               (copy-file cert (string-append out "/"
                                              (strip-store-file-name cert))))
             (list root-rsa root-ecdsa
                   intermediate-rsa intermediate-ecdsa
                   backup-rsa backup-ecdsa))

           ;; Create hash symlinks suitable for OpenSSL ('SSL_CERT_DIR' and
           ;; similar.)
           (chdir (string-append %output "/etc/ssl/certs"))
           (invoke (string-append perl "/bin/perl")
                   (string-append openssl "/bin/c_rehash")
                   ".")))))
    (native-inputs
     (list openssl perl))                           ;for 'c_rehash'
    (inputs
     `(; The Let's Encrypt root certificate, "ISRG Root X1".
       ("isrgrootx1.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/isrgrootx1.pem")
           (sha256
            (base32
             "1la36n2f31j9s03v847ig6ny9lr875q3g7smnq33dcsmf2i5gd92"))))
      ; Upcoming ECDSA Let's Encrypt root certificate, "ISRG Root X2"
      ; Let's Encrypt describes it as "Active, limited availability"
      ("isrgrootx2.pem"
        ,(origin
           (method url-fetch)
           (uri "https://letsencrypt.org/certs/isrg-root-x2.pem")
           (sha256
            (base32
             "04xh8912nwkghqydbqvvmslpqbcafgxgjh9qnn0z2vgy24g8hgd1"))))
      ;; "Let’s Encrypt Authority R3", the active Let's Encrypt intermediate
      ;; RSA certificate.
      ("letsencryptauthorityr3.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-r3.pem")
          (sha256
           (base32
            "0clxry49rx6qd3pgbzknpgzywbg3j96zy0227wwjnwivqj7inzhp"))))
      ;; "Let’s Encrypt Authority E1", the active Let's Encrypt intermediate
      ;; ECDSA certificate.
      ("letsencryptauthoritye1.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-e1.pem")
          (sha256
           (base32
            "1zwrc6dlk1qig0z23x6x7fib14rrw41ccbf2ds0rw75zccc59xx0"))))
      ;; "Let’s Encrypt Authority R4", the backup Let's Encrypt intermediate
      ;; RSA certificate.  This will be used for disaster recovery and will only be
      ;; used should Let's Encrypt lose the ability to issue with "Let’s
      ;; Encrypt Authority R3".
      ("letsencryptauthorityr4.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-r4.pem")
          (sha256
           (base32
            "09bzxzbwb9x2xxan3p1fyj1pi2p5yks0879gwz5f28y9mzq8vmd8"))))
      ;; "Let’s Encrypt Authority E2", the backup Let's Encrypt intermediate
      ;; ECDSA certificate.  This will be used for disaster recovery and will
      ;; only be used should Let's Encrypt lose the ability to issue with "Let’s
      ;; Encrypt Authority E1".
      ("letsencryptauthoritye2.pem"
       ,(origin
          (method url-fetch)
          (uri "https://letsencrypt.org/certs/lets-encrypt-e2.pem")
          (sha256
           (base32
            "1wfmsa29lyi9dkh6xdcamb2rhkp5yl2ppnsgrzcrjl5c7gbqh9ml"))))))
    (home-page "https://letsencrypt.org/certificates/")
    (synopsis "Let's Encrypt root and intermediate certificates")
    (description "This package provides a certificate store containing only the
Let's Encrypt root and intermediate certificates.  It is intended to be used
within Guix.")
    (license license:public-domain)))
