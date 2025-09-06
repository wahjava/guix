;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Stefan Reichoer <stefan@xsteve.at>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages python-apps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:))

;; This module is aimed at python leaves that are apps, i.e. supposed to be used
;; only on the CLI or graphically, not through their python libraries.
;; If possible, it should not be imported anywhere.

(define-public date2name
  (let ((commit "6c8f37277e8ec82aa50f90b8921422be30c4e798")
        (revision "1"))
    (package
      (name "date2name")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/novoid/date2name")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vq96b7n16d932nyfhnzwdwxff0zrqanidmwr4cxj2p67ad9y3w7"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f ;no tests
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build)
            (replace 'install
              (lambda _
                (let* ((bindir (string-append #$output "/bin"))
                       (binary (string-append bindir "/date2name")))
                  (mkdir-p bindir)
                  (copy-file "date2name/__init__.py" binary)
                  (chmod binary #o555)))))))
      (native-inputs (list python-setuptools-next))
      (synopsis "Handling time-stamps and date-stamps in file names")
      (description
       "By default, date2name gets the modification time of matching files and
directories and adds a datestamp in standard ISO 8601+ format YYYY-MM-DD at
the beginning of the file or directory name.")
      (home-page "https://github.com/novoid/date2name")
      (license license:gpl3+))))

(define-public glances
  (package
    (name "glances")
    (version "4.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nicolargo/glances")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00xyixi3wrajmkmqgd1rlaqypi6c1wskm6q0xbrw2k1zc7wi3kxl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-update-checks
            (lambda _
              ;; Glances phones PyPI for weekly update checks by default.
              ;; Disable these.  The user can re-enable them if desired.
              (substitute* "glances/outdated.py"
                (("^(.*)self\\.load_config\\(config\\)\n" line
                  indentation)
                 (string-append indentation
                                "self.args.disable_check_update = True\n"
                                line)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; XXX: Taken from tox.ini.
                (invoke "python" "unittest-core.py")))))))
    (native-inputs (list python-pytest python-setuptools-next))
    (propagated-inputs (list python-defusedxml python-orjson python-packaging
                             python-psutil))
    (home-page "https://github.com/nicolargo/glances")
    (synopsis "Cross-platform curses-based monitoring tool")
    (description
     "Glances is a curses-based monitoring tool for a wide variety of platforms.
     Glances uses the PsUtil library to get information from your system.  It
     monitors CPU, load, memory, network bandwidth, disk I/O, disk use, and more.")
    (license license:lgpl3+)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
