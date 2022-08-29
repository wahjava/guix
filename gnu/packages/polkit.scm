;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Huang Ying <huang.ying.caritas@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2022 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages polkit)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix memoization)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:export (polkit))

(define-public polkit
  (package
    (name "polkit")
    (version "121")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/polkit/releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1apz3bh7nbpmlp1cr00pb8z8wp0c7yb23ninb959jz3r38saxiwx"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build syscalls))
      #:modules '((guix build meson-build-system)
                  (guix build syscalls)
                  (guix build utils)
                  (ice-9 match))
      #:configure-flags
      #~(list "--sysconfdir=/etc"
              "-Dman=true"
              "-Dtests=true"
              ;; Work around cross-compilation failure.  The build system
              ;; probes for the _target_ gobject-introspection, but if we
              ;; change it to native, Meson fails with:
              ;;   ERROR: Pkg-config binary for machine
              ;;   MachineChoice.BUILD not found, giving up.
              ;; Just disable introspection for now.
              #$@(if (%current-target-system)
                     '("-Dintrospection=false")
                     '()))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-install-time-etc-directory
            ;; This is so that the default example rules files can be
            ;; installed along the package; otherwise it would fail attempting
            ;; to write to /etc.  Unlike with GNU Autotools, Meson can't
            ;; override the pkgsysconfdir value at install time; instead, we
            ;; rewrite the pkgsysconfdir references in the build system to
            ;; point to #$output/etc.
            (lambda _
              (substitute* "meson.build"
                (("pk_sysconfdir = get_option\\('sysconfdir')")
                 (format #f "pk_sysconfdir = '~a'"
                         (string-append #$output "/etc"))))))
          (add-before 'check 'patch-bash
            (lambda _
              (substitute* (list "subprojects/mocklibc-1.0/bin/mocklibc"
                                 (string-append "../polkit-v." #$version
                                                "/test/data/etc/passwd")
                                 (string-append "../polkit-v." #$version
                                                "/test/data/etc/polkit-1"
                                                "/rules.d/10-testing.rules"))
                (("/bin/(bash|false|true)" _ command)
                 (which command)))))
          (replace 'check
            (lambda* (#:key tests? test-options #:allow-other-keys)
              (when tests?
                ;; Run the test suite through tini to ensure signals are
                ;; properly handled and zombie processes reaped.
                (match (primitive-fork)
                  (0                    ;child process
                   (set-child-subreaper!)
                   ;; Use tini so that signals are properly handled and
                   ;; doubly-forked processes get reaped; otherwise,
                   ;; python-dbusmock would waste time polling for the dbus
                   ;; processes it spawns to be reaped, in vain.
                   (apply execlp "tini" "--"
                          "meson" "--" "test" "-t" "0" "--print-errorlogs"
                          test-options))
                  (pid
                   (match (waitpid pid)
                     ((_ . status)
                      (unless (zero? status)
                        (error "`meson test' exited with status"
                               status))))))))))))
    (inputs
     (list duktape expat elogind linux-pam nspr))
    (propagated-inputs
     (list glib))                       ;required by polkit-gobject-1.pc
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;for glib-mkenums
           docbook-xsl                  ;for man page generation
           gobject-introspection
           libxml2                      ;for XML_CATALOG_FILES
           libxslt                      ;for man page generation
           perl
           pkg-config
           python
           python-dbusmock
           tini))
    (home-page "https://www.freedesktop.org/wiki/Software/polkit/")
    (synopsis "Authorization API for privilege management")
    (description "Polkit is an application-level toolkit for defining and
handling the policy that allows unprivileged processes to speak to
privileged processes.  It is a framework for centralizing the decision
making process with respect to granting access to privileged operations
for unprivileged applications.")
    (license lgpl2.0+)))

(define-public polkit-qt
  (package
    (name "polkit-qt")
    (version "1-0.114.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kde/stable/polkit-qt-1/"
                   "polkit-qt-" version ".tar.xz"))
             (sha256
              (base32
               "0zlhwgkqn8g0rkjc7c5n7fbhyyl4jcv0rg5zlbzrb0l88ljg5c1f"))))
    (build-system cmake-build-system)
    (inputs
     (list polkit))
    (propagated-inputs
     (list qtbase-5))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:configure-flags (list (string-append "-DCMAKE_INSTALL_RPATH="
                                              (assoc-ref %outputs "out")
                                              "/lib:"
                                              (assoc-ref %outputs "out")
                                              "/lib64"))
       #:tests? #f)) ; there is a test subdirectory, but no test target
    (home-page "https://api.kde.org/kdesupport-api/polkit-qt-1-apidocs/")
    (synopsis "Qt frontend to the polkit library")
    (description "Polkit-qt is a library that lets developers use the
PolicyKit API through a Qt-styled API.  It is mainly a wrapper around
QAction and QAbstractButton that lets you integrate those two component
easily with PolicyKit.")
    (license lgpl2.0+)))

(define-public polkit-gnome
  (package
    (name "polkit-gnome")
    (version "0.105")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/"
                                  name "/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0sckmcbxyj6sbrnfc5p5lnw27ccghsid6v6wxq09mgxqcd4lk10p"))))
    (build-system gnu-build-system)
    (inputs (list gtk+ polkit))
    (native-inputs (list intltool pkg-config))
    (synopsis "Legacy polkit authentication agent for GNOME")
    (description "PolicyKit-gnome provides a D-Bus session bus service
that is used to bring up authentication dialogs used for obtaining
privileges.")
    (home-page "https://www.freedesktop.org/wiki/Software/polkit/")
    (license lgpl2.0+)))
