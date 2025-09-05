;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
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

(define-module (gnu packages ftp)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ gpl3+ clarified-artistic)
                #:prefix license:)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml))

(define-public lftp
  (package
    (name "lftp")
    (version "4.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lavv17/lftp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hzy2g426az6y5gh2pzkz8bd0z744ibnvs72iqwy902y7lhdil0f"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf-2.71)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ;; required by gnulib to fix parse-datetime.c compilation error
       ("bison" ,bison)
       ("gnulib"
        ,(let ((commit "949989513e631b61196f11fd8d647d4dc77d7f40"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "git://git.savannah.gnu.org/gnulib.git")
                   (commit commit)))
             (sha256
              (base32 "0jk336yxfzldi0nsni0q3cxa7lqxb8lzmiisrr594v8cfvcprqn9"))
             (file-name (git-file-name "gnulib" commit)))))
       ("gperf" ,gperf)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list zlib readline gnutls))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-gnulib
                 (lambda _
                   (copy-recursively #$(this-package-native-input "gnulib")
                                     "gnulib")))
               (delete 'bootstrap)
               (add-after 'patch-source-shebangs 'bootstrap
                 (lambda _
                   (invoke "sh" "bootstrap"
                           "--no-git"
                           "--gnulib-srcdir=gnulib")))
               ;; Disable tests that require network access, which is most of them.
               (add-before 'check 'disable-impure-tests
                 (lambda _
                   (substitute* "tests/Makefile"
                     (("(ftp-cls-l|ftp-list|http-get)\\$\\(EXEEXT\\)") "")
                     (("lftp-https-get ") "")))))
           #:configure-flags
           #~(list (string-append "--with-readline="
                                  #$(this-package-input "readline")))))
    (home-page "https://lftp.yar.ru/")
    (synopsis "Command-line file transfer program")
    (description
     "LFTP is a sophisticated FTP/HTTP client, and a file transfer program
supporting a number of network protocols.  Like Bash, it has job control and
uses the Readline library for input.  It has bookmarks, a built-in mirror
command, and can transfer several files in parallel.  It was designed with
reliability in mind.")
    (license gpl3+)))

(define-public ncftp
  (package
    (name "ncftp")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ncftp.com/public_ftp/ncftp/ncftp-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "11k375kcimd8fxb6zxdkqy2qfs927mpnvi21iqncibxdqa2gh83r"))
              (patches
               (search-patches
                "ncftp-reproducible.patch"
                "ncftp-paths.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Use the right 'rm' and 'ls'.
                  (substitute* (cons "configure"
                                     (find-files "."
                                                 "^(Makefile\\.in|.*\\.sh)$"))
                    (("/bin/(rm|ls)" _ command)
                     command))

                  ;; This is free software, avoid any confusion.
                  (substitute* (find-files "." "\\.c$")
                    (("a freeware program")
                     "free software"))
                  #t))))
    (properties
     `((release-monitoring-url . "https://www.ncftp.com/download/")))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; This is an old 'configure' script that doesn't
                    ;; understand variables passed as arguments.
                    (let ((out (assoc-ref outputs "out")))
                      (setenv "CONFIG_SHELL" (which "sh"))
                      (setenv "SHELL" (which "sh"))
                      (setenv "CFLAGS" "-fcommon")
                      (invoke "./configure"
                              (string-append "--prefix=" out))))))
       #:tests? #f)) ;there are no tests
    (inputs (list ncurses))
    (home-page "https://www.ncftp.com/ncftp/")
    (synopsis "Command-line File Transfer Protocol (FTP) client")
    (description
     "NcFTP Client (or just NcFTP) is a set of command-line programs to access
File Transfer Protocol (FTP) servers.  This includes @code{ncftp}, an interactive
FTP browser, as well as non-interactive commands such as @code{ncftpput} and
@code{ncftpget}.")
    (license clarified-artistic)))


(define-public weex
  (package
    (name "weex")
    (version "2.8.2")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/weex/weex/"
                         "/weex_" version ".tar.gz"))
        (sha256
          (base32
            "1ir761hjncr1bamaqcw9j7x57xi3s9jax3223bxwbq30a0vsw1pd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf gettext-minimal))
    (home-page "https://weex.sourceforge.net/")
    (synopsis "Non-interactive client for FTP synchronization")
    (description
     "Weex is a utility designed to automate the task of remotely
maintaining a web page or other FTP archive.  It synchronizes a set of
local files to a remote server by performing uploads and remote deletes
as required.")
    (license gpl2+)))

(define-public libfilezilla
  (let ((revision 11287))
    (package
      (name "libfilezilla")
      (version "0.51.0")
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "https://svn.filezilla-project.org/svn/libfilezilla/trunk")
               (revision revision)))
         (file-name (string-append name "-" version "-" (number->string revision)))
         (sha256
          (base32 "1ylapyyi1lj41w3c73dzq2r43vwmmk820z2gd0qfs3sdi9wb95z1"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags (list "--disable-static")))
      (native-inputs
       (list cppunit
             gettext-minimal
             libxcrypt
             pkg-config
             autoconf
             automake
             libtool))
      (inputs
       (list gnutls nettle))
      (home-page "https://lib.filezilla-project.org")
      (synopsis "Cross-platform C++ library used by Filezilla client")
      (description
       "This package provides some basic functionality to build high-performing,
platform-independent programs.

Some of the highlights include:
@itemize
@item
A type-safe, multi-threaded event system that's simple to use yet efficient.
@item
Timers for periodic events.
@item
A @code{datetime} class that not only tracks timestamp but also their accuracy,
which simplifies dealing with timestamps originating from different sources.
@item
Simple process handling for spawning child processes with redirected input and
output.
@end itemize\n")
      (license gpl2+))))

(define-public filezilla
  (let ((revision 11290))
    (package
      (name "filezilla")
      (version "3.69.2")
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "https://svn.filezilla-project.org/svn/FileZilla3/trunk")
               (revision revision)))
         (sha256
          (base32 "0pz8fhzgm4kyax64bkkpsq3p1lcypjacr05bvk4wxp80lfqiv32k"))))
      (build-system gnu-build-system)
      (arguments
        ;; Don't let filezilla phone home to check for updates.
       '(#:configure-flags '("--disable-autoupdatecheck")))
      (native-inputs
       (list autoconf automake cppunit gettext-minimal libtool pkg-config xdg-utils))
      (inputs
       (list boost
             dbus
             gnutls
             gtk+
             libfilezilla
             libidn
             nettle
             pugixml
             sqlite
             wxwidgets))
      (home-page "https://filezilla-project.org")
      (synopsis "Full-featured graphical FTP/FTPS/SFTP client")
      (description
       "Filezilla client supports FTP, FTP over SSL/TLS (FTPS),
SSH File Transfer Protocol (SFTP), HTTP/1.1, SOCKS5, FTP-Proxy, IPv6
and others features such as bookmarks, drag and drop, filename filters,
directory comparison and more.")
      (license gpl2+)
      (properties '((upstream-name . "FileZilla"))))))

(define-public vsftpd
  (package
    (name "vsftpd")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://security.appspot.com/downloads/"
                           "vsftpd-" version ".tar.gz"))
       (sha256
        (base32 "1lwipiq8q9qzvwv6f418fbvagpz0p6v0jjplkvcsc2sb8np05di6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; Work around, e.g., “ssl.c:149:7: error: ‘EC_KEY_free’ is
             ;; deprecated: Since OpenSSL 3.0 [-Werror=deprecated-declarations]”
             "CFLAGS=-Wno-deprecated-declarations"
             ;; vsf_findlibs.sh looks only for hard-coded {/usr,}/lib file names
             ;; that will never exist on Guix.  Manage libraries ourselves.
             "LDFLAGS=-lcap -lpam"
             "INSTALL=install -D")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'build-SSL
           (lambda _
             (substitute* "builddefs.h"
               (("#undef (VSF_BUILD_SSL)" _ symbol)
                (string-append "#define " symbol)))))
         (add-after 'unpack 'append-make-flags
           (lambda _
             (substitute* "Makefile"
               (("(CFLAGS|LDFLAGS)[[:blank:]]*=" _ variable)
                (format #f "UPSTREAM_~a +=" variable))
               (("\\$\\((CFLAGS|LDFLAGS)\\)" _ variable)
                (format #f "$(UPSTREAM_~a) $(~@*~a)" variable)))))
         (add-after 'unpack 'patch-installation-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))))
         (delete 'configure))))         ; no configure script
    (inputs
     (list libcap linux-pam openssl))
    (synopsis "Small FTP server with a focus on security")
    (description
     "The Very Secure File Transfer Protocol Daemon or @command{vsftpd} is a
server that listens on a TCP socket for clients and gives them access to local
files via @acronym{FTP, the File Transfer Protocol}.  Security is a goal; not a
guarantee.")
    (home-page "https://security.appspot.com/vsftpd.html")
    (license gpl2)))                    ; with OpenSSL exception

(define-public proftpd
  (package
    (name "proftpd")
    (version "1.3.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/proftpd/proftpd")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "proftpd-no-install-user.patch"))
       (sha256
        (base32
         "0zgx652qdwj2xkc7218v74x919dxmcs3q5s61v1kpdcw8f9v7370"))))
    (build-system gnu-build-system)
    (inputs
     (list libsodium
           libxcrypt
           linux-pam
           ncurses
           openssl
           perl
           zlib
           libcap))
    (arguments
     (list
      #:configure-flags #~(list "--enable-openssl"
                                "--with-modules=mod_sftp")))
    (synopsis "Highly configurable FTP server")
    (description
     "ProFTPD grew out of the desire to have a secure and configurable FTP
server admiring Apache web server. It's an independent source tree from the
ground up.")
    (home-page "http://www.proftpd.org/")
    (license
     (list gpl2+ gpl3+ gpl2
           license:x11
           license:expat
           license:public-domain
           license:bsd-1
           license:bsd3))))
