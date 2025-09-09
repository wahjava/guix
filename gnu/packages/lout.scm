;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages lout)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ghostscript))

(define-public lout
  (package
    (name "lout")
    (version "3.43.2")

    ;; The original version at <https://savannah.nongnu.org/projects/lout/> is
    ;; effectively abandoned, not receiving build fixes and the likes, and
    ;; most distros have been providing this one instead.
    (home-page "https://github.com/william8000/lout")

    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lvk1ffzgydgj4csaclrh3a9nwla061clgf8lda6n47masg12qzi"))))
    (build-system gnu-build-system)     ; actually, just a makefile
    (outputs '("out" "doc"))
    (native-inputs
     (list ghostscript))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1))        ; we need SRFI-1
       #:tests? #f                      ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; This package is a bit tricky, because it doesn't follow the GNU
         ;; Build System rules.  Instead, it has a makefile that has to be
         ;; patched to set the prefix, etc., and it has no makefile rules to
         ;; build its documentation.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (substitute* "makefile"
                 (("^PREFIX[[:blank:]]*=.*$")
                  (string-append "PREFIX = " out "\n"))
                 (("^LOUTLIBDIR[[:blank:]]*=.*$")
                  (string-append "LOUTLIBDIR = " out "/lib/lout\n"))
                 (("^LOUTDOCDIR[[:blank:]]*=.*$")
                  (string-append "LOUTDOCDIR = " doc "/share/doc/lout\n"))
                 (("^MANDIR[[:blank:]]*=.*$")
                  (string-append "MANDIR = " out "/man\n")))
               (mkdir out)
               (mkdir (string-append out "/bin"))
               (mkdir (string-append out "/lib"))
               (mkdir (string-append out "/man"))
               (mkdir-p (string-append doc "/share/doc/lout"))
               #t)))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "installman")
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "doc")))
               (setenv "PATH"
                       (string-append (assoc-ref outputs "out")
                                      "/bin:" (getenv "PATH")))
               (with-directory-excursion "doc"
                 (every (lambda (doc)
                          (format #t "doc: building `~a'...~%" doc)
                          (with-directory-excursion doc
                            (let ((file (string-append out "/share/doc/lout/"
                                                       doc ".ps")))
                              (unless (file-exists? "outfile.ps")
                                (invoke "lout" "-r4" "-o"
                                        "outfile.ps" "all"))
                              (copy-file "outfile.ps" file)
                              (invoke "ps2pdf"
                                      "-dPDFSETTINGS=/prepress"
                                      "-sPAPERSIZE=a4"
                                      file
                                      (string-append out "/share/doc/lout/"
                                                     doc ".pdf")))))
                        '("design" "expert" "slides" "user")))
               #t))))))
    (synopsis "Document layout system")
    (description
     "The Lout document formatting system reads a high-level description of
a document similar in style to LaTeX and produces a PostScript or plain text
output file.

Lout offers an unprecedented range of advanced features, including optimal
paragraph and page breaking, automatic hyphenation, PostScript EPS file
inclusion and generation, equation formatting, tables, diagrams, rotation and
scaling, sorted indexes, bibliographic databases, running headers and
odd-even pages, automatic cross referencing, multilingual documents including
hyphenation (most European languages are supported), formatting of computer
programs, and much more, all ready to use.  Furthermore, Lout is easily
extended with definitions which are very much easier to write than troff of
TeX macros because Lout is a high-level, purely functional language, the
outcome of an eight-year research project that went back to the
beginning.")
    (license license:gpl3+)))
