;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Jørgen Kvalsvik <j@lambda.is>
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

(define-module (guix go-mod-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix download)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 regex)
  #:autoload (guix build-system gnu) (standard-packages)
  #:export (go-mod-reference
            go-mod-reference?
            go-mod-reference-path
            go-mod-reference-version
            go-mod-fetch))

;;; Commentary:
;;;
;;; An <origin> method that fetches a go module [1] from a GOPROXY.  A go
;;; module is usually identified as a vcs (usually git) repository,
;;; e.g. github.com/calmh/du or golang.org/x/net.
;;;
;;; This is mostly a regular http(s) fetch some custom url building. Unless
;;; goproxy is specified, it fetches from the default goproxy
;;; https://proxy.golang.org.  This is mostly just a convenience -- the same
;;; code could be fetched directly, but sometimes libraries are only
;;; practically available through a goproxy. Such a module would be
;;; https://pkg.go.dev/buf.build/gen/go/bufbuild/protovalidate/protocolbuffers/go
;;;
;;; [1] https://go.dev/ref/mod
;;;
;;; Code:

(define-record-type* <go-mod-reference>
  go-mod-reference make-go-mod-reference
  go-mod-reference?
  (path go-mod-reference-path)
  (version go-mod-reference-version)
  (goproxy go-mod-reference-goproxy (default "https://proxy.golang.org")))

(define (default-unzip)
  "Return the 'unzip' package.  This is a lazy reference so that we don't
depend on (gnu packages compression)."
  (module-ref (resolve-interface '(gnu packages compression)) 'unzip))

;; Fetch a go module e.g. golang.org/x/net from a goproxy.
(define* (go-mod-fetch ref hash-algo hash
                       #:optional name
                       #:key (system (%current-system))
                       (guile (default-guile))
                       (unzip (default-unzip)))
  (define inputs
    `(("unzip" ,unzip)
      ,@(standard-packages)))

  (define (go-path-escape path)
    "Escape a module path by replacing every uppercase letter with an
exclamation mark followed with its lowercase equivalent, as per the module
Escaped Paths specification (see:
https://godoc.org/golang.org/x/mod/module#hdr-Escaped_Paths)."
    (define (escape occurrence)
      (string-append "!" (string-downcase (match:substring occurrence))))
    (regexp-substitute/global #f "[A-Z]" path 'pre escape 'post))

  (define modules
    (source-module-closure '((guix build utils))))

  (define (build mod.zip)
    (with-imported-modules modules
      #~(begin
          (use-modules (guix build utils))
          (let* ((pkg-path (getenv "go-mod path"))
                 (pkg-version (getenv "go-mod version"))
                 (pkg-root (string-append pkg-path "@v" pkg-version))
                 (go.mod (string-append pkg-root "/go.mod")))

            (invoke (string-append #+unzip "/bin/unzip") "-q" #$mod.zip)
            ;; The sources in the zip are in the subdir
            ;; $path@v$version/, but we want our sources at root.
            (copy-recursively pkg-root #$output)))))

    (define path-as-store-name
      (string-append
       (string-replace-substring (go-mod-reference-path ref) "/" "-")
       "-" (go-mod-reference-version ref)))

    (define url/zip
      (format #f "~a/~a/@v/v~a.zip"
              (go-mod-reference-goproxy ref)
              (go-path-escape (go-mod-reference-path ref))
              (go-mod-reference-version ref)))

    (mlet* %store-monad ((guile-for-build (package->derivation guile system))
                         (mod (url-fetch url/zip hash-algo hash
                                         (or name (string-append path-as-store-name ".zip"))
                                         #:system system
                                         #:guile guile)))
      (gexp->derivation (or name path-as-store-name) (build mod)
                        #:script-name "go-mod-fetch"
                        #:env-vars
                        `(("go-mod path" . ,(go-mod-reference-path ref))
                          ("go-mod version" . ,(go-mod-reference-version ref)))
                        #:leaked-env-vars '("http_proxy" "https_proxy"
                                            "LC_ALL" "LC_MESSAGES" "LANG"
                                            "COLUMNS")
                        #:system system
                        #:local-build? #t)))
