;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lapearldot <lapearldot@disroot.org>
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

(define-module (gnu packages nl-translation)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)

  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages language)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages check)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public python-argostranslate
  (package
    (name "python-argostranslate")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/argosopentech/argos-translate")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gwpzv002m5h7gnglgvcld6zyijjp4d836wpql4h2irf0w67cl9q"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-test-home
            (lambda _
              (setenv "HOME"
                      (getcwd)))))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-ctranslate2 python-packaging
                             python-sacremoses python-sentencepiece-0.2
                             python-stanza))
    (native-inputs (list python-setuptools python-wheel python-pytest))
    (home-page "https://www.argosopentech.com")
    (synopsis
     "Open-source neural machine translation library based on OpenNMT's
CTranslate2")
    (description
     "Argos Translate uses OpenNMT for translations and can be used as either
a Python library, command-line, or GUI application.  Argos Translate supports
installing language model packages which are zip archives with a
\".argosmodel\" extension containing the data needed for translation.
LibreTranslate is an API and web-app built on top of Argos Translate.")
    (license license:expat)))

(define-public python-argostranslategui
  (package
    (name "python-argostranslategui")
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/argosopentech/argos-translate-gui")
             (commit
              ;; no releases
              "06aafde3faf1709080471c94914382a566f6f775")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g251fgwwbk96f8n11c8gzidqgfkrkwpldxk9zq0kkhm729b9yyw"))))
    (arguments
     (list
      ;; lack of ssl certificate bundle error
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-test-home
            (lambda _
              (setenv "HOME"
                      (getcwd)))))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-argostranslate python-pyqt))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.argosopentech.com")
    (synopsis "Graphical user interface for Argos Translate")
    (description "Graphical user interface for Argos Translate.")
    (license license:expat)))

(define-public python-translatehtml
  (package
    (name "python-translatehtml")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/argosopentech/translate-html")
             (commit
              ;; no releases
              "d1bdce42901df22e40f29e42bc4fd5784e588ee5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1imga5vpfkysw2fh4yc7j6r5w14jjd2vglrqi9iv0x0z1fcn7s20"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-test-home
            (lambda _
              (setenv "HOME"
                      (getcwd)))))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-argostranslate python-beautifulsoup4))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://www.argosopentech.com")
    (synopsis "Translate HTML using Beautiful Soup and Argos Translate")
    (description "Translate HTML using Beautiful Soup and Argos Translate.")
    (license license:expat)))

(define-public python-argos-translate-files
  (package
    (name "python-argos-translate-files")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LibreTranslate/argos-translate-files")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01v51idnlrjqqzlnczzwzf679n8dn4yyharl8wchcgsw2agxghby"))
       (snippet #~(substitute* "requirements.txt"
                    (("^(lxml>=)4.9.2" all stem)
                     (string-append stem "4.9.1"))))
       (modules `((guix build utils)))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-test-home
            (lambda _
              (setenv "HOME"
                      (getcwd)))))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-argostranslate python-beautifulsoup4
                             python-lxml python-pysrt python-translatehtml))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/LibreTranslate/argos-translate-files")
    (synopsis "Translate files with Argos Translate")
    (description "Translate files with Argos Translate.")
    (license license:expat)))

(define-public python-lexilang
  (package
    (name "python-lexilang")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lexilang" version))
       (sha256
        (base32 "11lfgp7dp3v4crldyj4ijk24q2b0c4g229i8arnwd1hlbqjz6srq"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/LibreTranslate/LexiLang")
    (synopsis "Simple, fast dictionary-based language detector")
    (description "Simple, fast dictionary-based language detector.")
    (license license:agpl3)))

(define-public python-libretranslate
  (package
    (name "python-libretranslate")
    (version "1.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libretranslate" version))
       (sha256
        (base32 "1k0x5l2rc1qsv1nrjgjaa1gcz94v8ly2w9nagfccvfx6kc5yvg93"))))
    (build-system pyproject-build-system)
    ;; avoid joint propagation of python-flask 2 and newer versions
    (inputs (list python-flask-2
                  python-flask-2-limiter
                  python-flask-2-swagger
                  python-flask-2-swagger-ui))
    (propagated-inputs (list python-appdirs
                             python-apscheduler
                             python-argos-translate-files
                             python-argostranslate
                             python-expiringdict
                             ;; these should probably be brought in line with flask 2
                             python-flask-babel
                             python-flask-session
                             python-itsdangerous
                             python-langdetect
                             python-lexilang
                             python-morfessor
                             python-numpy
                             python-packaging
                             python-polib
                             python-prometheus-client
                             python-redis
                             python-requests
                             python-pytorch
                             python-translatehtml
                             python-waitress
                             python-werkzeug))
    (native-inputs (list python-hatchling
                         ;; large dep count
                         ;; python-pip-audit
                         python-pytest
                         python-pytest-cov
                         python-types-requests))
    (arguments
     (list
      ;; tests check for internet
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'wipe-init
            (lambda _
              ;; this behaviour causes the modules of the following
              ;; names to be obscured in place of their member
              ;; functions.
              ;; this breaks the "create-entrypoints" phase
              (substitute* "libretranslate/__init__.py"
                (("from \\.(main|manage).*$")
                 ""))))
          (add-before 'check 'set-test-home
            (lambda _
              (setenv "HOME"
                      (getcwd)))))))
    (home-page "https://github.com/LibreTranslate/LibreTranslate")
    (synopsis "Free and Open Source Machine Translation API")
    (description
     "Free and Open Source Machine Translation API, entirely
self-hosted.  Unlike other APIs, it doesn't rely on proprietary providers such
as Google or Azure to perform translations.  Instead, its translation engine
is powered by the open source Argos Translate library.")
    (license license:agpl3)))
