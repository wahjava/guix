;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Stefan Reichoer <stefan@xsteve.at>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2022 Matthew James Kraai <kraai@ftbfs.org>
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
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
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

(define-public i3-autotiling
  (package
    (name "i3-autotiling")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nwg-piotr/autotiling")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ag3zz4r3cwpj769m2aw3l8yj93phsydzfz02dig5z81cc025rck"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;no tests
    (native-inputs (list python-setuptools-next))
    (propagated-inputs (list python-i3ipc))
    (home-page "https://github.com/nwg-piotr/autotiling")
    (synopsis "Automatically tile windows in i3 and Sway")
    (description
     "Script for Sway and i3 to automatically switch the horizontal/vertical
 window split orientation.")
    (license license:gpl3)))

(define-public offlate
  (package
    (name "offlate")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://framagit.org/tyreunom/offlate")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sx5cv8pamyw1m089b6x8ykaxdkx26jk9cblhbzlf0m3ckz52jik"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'generate-fonts
            (lambda _
              (invoke "make" "fonts")))
          (add-before 'build 'generate-translations
            (lambda _
              (invoke "make" "update-langs"))))))
    (propagated-inputs
     (list python-android-stringslib
           python-dateutil
           python-gitlab
           python-lxml
           python-polib
           python-pycountry
           python-pyenchant
           python-pygit2
           python-pygithub
           python-pyqt
           python-requests
           python-ruamel.yaml
           python-translate-toolkit
           python-translation-finder
           python-watchdog))
    (native-inputs (list qttools-5 fontforge python-setuptools-next))
    (home-page "https://framagit.org/tyreunom/offlate")
    (synopsis "Offline translation interface for online translation tools")
    (description
     "Offlate offers a unified interface for different translation file
formats, as well as many different online translation platforms.  You can use
it to get work from online platforms, specialized such as the Translation
Project, or not such a gitlab instance when your upstream doesn't use any
dedicated platform.  The tool proposes a unified interface for any format and
an upload option to send your work back to the platform.")
    (license license:gpl3+)))

(define-public online-judge-tools
  (package
    (name "online-judge-tools")
    (version "11.5.1")
    ;; Source distributions are not uploaded to PyPI.
    ;; https://pypi.org/project/online-judge-tools/11.5.1/#files
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/online-judge-tools/oj")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkzmmjgjb6lyrzq1ip54cpnp7al9a7mcyjyi5vx58bvnx3q0c6m"))
       (patches (search-patches "online-judge-tools.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests require network connections
      #~(list "--ignore=tests/command_version.py")))
    (native-inputs (list python-pytest python-setuptools-next))
    (inputs (list time))
    (propagated-inputs (list python-online-judge-api-client python-colorama
                             python-requests))
    (home-page "https://github.com/online-judge-tools/oj")
    (synopsis "Command to help solving problems on various online judges")
    (description
     "@command{oj} is a command line tool to help solving problems on
various online judges.  This command automates downloading sample
cases, generating additional test cases, testing for your code, and
submitting it.")
    (license license:expat)))

(define-public ptpython
  (package
    (name "ptpython")
    (version "3.0.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonathanslenders/ptpython")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qbj7d4qkzl8l05kpmm19953lmqk379i17ab8g3sfnmfpsy3ji5m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: There is a tests/run_tests.py file but all it does is a
      ;; sanity check.
      #:tests? #f))
    (native-inputs (list python-setuptools-next))
    (propagated-inputs
     (list python-appdirs python-jedi python-prompt-toolkit python-pygments))
    (home-page "https://github.com/jonathanslenders/ptpython")
    (synopsis "Python Read-Eval-Print-Loop with nice IDE-like features")
    (description
     "ptpython is a Python read-eval-print loop with IDE-like features.
It supports syntax highlighting, multiline editing, autocompletion, mouse,
color schemes, bracketed paste, Vi and Emacs keybindings, Chinese characters
etc.")
    (license license:bsd-3)))

(define-public pyzo
  (package
    (name "pyzo")
    (version "4.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyzo/pyzo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0agq171cz7y10cknjypwrvsvikja3w9d28hlr3kw5k2sdvfqnpam"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore-glob=pyzo/yoton/tests/*"     ; XXX: yoton is outdated.
              "--ignore=pyzo/codeeditor/_test.py"))) ; XXX: cannot import qt.
    (native-inputs (list python-pytest python-setuptools-next))
    (propagated-inputs (list python-pyqt))
    (home-page "https://pyzo.org")
    (synopsis "Python IDE for scientific computing")
    (description
     "Pyzo is a Python IDE focused on interactivity and introspection,
which makes it very suitable for scientific computing.  Its practical
design is aimed at simplicity and efficiency.

It consists of two main components, the editor and the shell, and uses
a set of pluggable tools to help the programmer in various ways.  Some
example tools are source structure, project manager, interactive help,
workspace...")
    (license license:bsd-2)))

(define-public s3cmd
  (package
    (name "s3cmd")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/s3tools/s3cmd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rdgwwmmp8mdxc84bxq6k9a7v7z2qgc3df47djzs2b84gw81dglx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ; XXX: Tests require network access.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'hide-wrapping
            (lambda _
              (substitute* "S3/MultiPart.py"
                (("sys\\.argv\\[0\\]")
                 "\"s3cmd\""))
              (substitute* "s3cmd"
                (("optparser\\.get_prog_name\\(\\)")
                 "\"s3cmd\""))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "run-tests.py")))))))
    (native-inputs (list python-setuptools-next))
    (inputs (list python-dateutil python-magic))
    (home-page "https://s3tools.org/s3cmd")
    (synopsis "Command line tool for S3-compatible storage services")
    (description
     "S3cmd is a command line tool for uploading, retrieving and managing data
in storage services that are compatible with the Amazon Simple Storage
Service (S3) protocol, including S3 itself.  It supports rsync-like backup,
GnuPG encryption, and more.  It also supports management of Amazon's
CloudFront content delivery network.")
    (license license:gpl2+)))

(define-public wfetch
  (let ((commit "e1cfa37814aebc9eb56ce994ebe877b6a6f9a715")
        (revision "2"))
    (package
      (name "wfetch")
      (version (git-version "0.1-pre" revision commit))
      (home-page "https://github.com/Gcat101/Wfetch")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dmr85plx8zr6s14ym3r32g6crwxghkval5a24ah90ijx4dbn5q5"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f ;no test suite
        #:phases
        #~(modify-phases %standard-phases
            (delete 'build)
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin"))
                      (share (string-append #$output "/share")))
                  (mkdir-p share)
                  (substitute* "wfetch/wfetch.py"
                    (("os.sep, 'opt', 'wfetch'")
                     (string-append "'" share "'")))
                  ;; The documentation expects the executable to be named
                  ;; 'wfetch', not 'wfetch.py'.
                  (rename-file "wfetch/wfetch.py" "wfetch/wfetch")
                  (install-file "wfetch/wfetch" bin)
                  (copy-recursively "wfetch/icons" share)))))))
      (native-inputs (list python-setuptools-next))
      (inputs (list python-pyowm python-fire python-termcolor python-requests))
      (synopsis "Command-line tool to display weather info")
      (description
       "This package provides a tool similar to Neofetch/pfetch, but for
weather: it can display the weather condition, temperature, humidity, etc.

To use it, you must first run:

@example
export WEATHER_CLI_API=@var{your OpenWeatherMap API key}
@end example
")
      (license license:gpl3+))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
