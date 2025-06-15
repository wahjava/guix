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

