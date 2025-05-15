(define-module (guix build go-module-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            go-module-build))

;;; Commentary:
;;;
;;; Build procedure for packages using the module aware Go build
;;; system.  The go build system aggressively tries to fetch dependencies
;;; or even compiler toolchains.  While it may be possible to convince it to
;;; not do that, we opt for not fighting it, and instead let it fetch
;;; everything it wants to, served from the local filesystem in directories we
;;; populate.
;;;
;;; The GOPROXY protocol [1] permits using file:// urls.  From the manual:
;;;
;;;   A module proxy is an HTTP server that can respond to GET requests for
;;;   paths specified below. The requests have no query parameters, and no
;;;   specific headers are required, so even a site serving from a fixed file
;;;   system (including a file:// URL) can be a module proxy.
;;;
;;; Go dependencies tend to be rigidly specified to very specific versions,
;;; with hashes, which the go build tooling will figure out.  This does not
;;; work too well with guix' model, where we want to specify dependencies more
;;; fludily (e.g. with input substitutions).  Go modules also tend to specify
;;; (minimum) toolchains which is not strictly necessary from a language
;;; feature perspective, which breaks builds with older compilers.
;;;
;;; To address these problems, we always write a fresh go.mod file based on
;;; the build-inputs.  There is no guarantee that there even is a go.mod file
;;; in the source, especially for older projects.  Go build uses this file to
;;; "download" from our just-assembled goproxy, which makes it happy.  This
;;; also clears any toolchain directive which makes the build accept the go
;;; compiler through build-inputs.  We populate the goproxy with just-in-time
;;; built zips, version, and info files.  This is a separate phase so that
;;; additional build steps can be added between building the proxy and running go
;;; build.
;;;
;;; The build system is compatible with go-build-system, in the sense that
;;; go-build-system can be used as build-inputs, and vice versa, because they
;;; both use the same $out/source/.
;;;
;;; We re-used compiled packages.  The Go build system creates a
;;; content-addressable build cache, which we install into build output, and
;;; use to seed downstream builds.  Go programs are (mostly) statically
;;; linked, so this is roughly equivalent of installing lib.a.  Note that this
;;; only works when the build-input is built with go-module-build-system.
;;;
;;; [1] https://go.dev/ref/mod#goproxy-protocol
;;;
;;; Code:

(define (find-single-file dir regex)
  "Find the file in DIR matching the REGEX, and fail unless there is
exactly one match."
  (let ((files (find-files dir regex #:directories? #f)))
    (unless (eq? 1 (length files))
      (error "Expected exactly one file matching pattern, found:" files))
    (car files)))

(define (go-path-escape path)
  "Escape a module path by replacing every uppercase letter with an
exclamation mark followed with its lowercase equivalent, as per the module
Escaped Paths specification (see:
https://godoc.org/golang.org/x/mod/module#hdr-Escaped_Paths)."
  (define (escape occurrence)
    (string-append "!" (string-downcase (match:substring occurrence))))
  (regexp-substitute/global #f "[A-Z]" path 'pre escape 'post))

(define (call-with-append-file path f)
  "call-with-output-file, but appends to the file if it exists rather
than truncating it"
  (let ((file (open-file path "a")))
    (f file)
    (close file)))

(define (set-cache-action-epoch f)
  "Set go build cache action entry timestamp to 0

The go build cache action entries (xxxx-a) record a timestamp, which
would break reproducibility of the build cache. Set it to all-zeros."
  ;; The file has 5 columns, tand the timestamp is the rightmost one
  ;; <version> <action-id> <output-id> <size> <timestamp>
  ;;
  ;; The timestamp seems to be in nanoseconds since epoch. We use
  ;; replacement to avoid potential problems with whitespace
  ;; sensitivity.
  (let* ((file-line (read-first-line f))
         (start-timestamp (string-skip-right file-line char-set:digit))
         (end-timestamp (string-length file-line))
         (base-line (substring file-line 0 start-timestamp))
         (zero-timestamp (make-string (- end-timestamp start-timestamp) #\0)))
    (call-with-output-file f
      (lambda (port)
        (format port "~a~a~%" base-line zero-timestamp)))))

(define (copy-nonlink-recursively src-dir dst-dir)
  "Recursively install files from src/ to dst/, skipping symlinks"
  (copy-recursively src-dir dst-dir
                    #:log #f
                    #:copy-file
                    (lambda (src dst)
                      (unless (symbolic-link? src)
                        (copy-file src dst)))))

(define (make-tags tags)
  "Construct a -tags argument list.

We accept tags both as a single string and a list of tags. go expects
-tags tag1,tag2,..."
  (cond ((and (list? tags) (not (null? tags)))
         (list "-tags" (string-join tags ",")))
        ((string? tags) (list "-tags" tags))
        (else '())))

(define (re-init-module module-path)
  "Create a fresh go.mod file, replacing an old one if it exists."
  ;; Wipe the go.mod if it exists, then create a new one. We might
  ;; not use the exact same input set (versions or even modules) as
  ;; upstream, e.g. when splitting an upstream package into multiple
  ;; parts.
  ;;
  ;; Delete all go.sum files, if they exist. We do our own
  ;; checksums so there is no safety here, and since our packages
  ;; are differently sourced (and maybe differently versioned) they
  ;; won't match upstream checksums.
  (when (file-exists? "go.mod") (delete-file "go.mod"))
  (when (file-exists? "go.sum") (delete-file "go.sum"))
  (invoke/quiet "go" "mod" "init" module-path))

(define (read-first-line f)
  "Read the first line in a file"
  (call-with-input-file f read-line))

(define (filter-go-inputs inputs)
  "Return the store paths of go library inputs.

Inputs is a list of ('pkg' 'store-path') pairs, and returns a list of
store paths.

((zip . /gnu/store/x1c9w6dnmk23mpdfg08zyq379q26nd88-zip-3.0)
 (unzip . /gnu/store/fmli224wbxrz1n0i2lhz6gy8a1ydcbp3-unzip-6.0)
 (go-github-com-stretchr-testify . /gnu/store/7p6zk3zka35g3699b9kfl0njzwykimjm-go-github-com-stretchr-testify-1.10.0)
 (go-golang-org-x-tools . /gnu/store/ax54x3d7fyywbppqvf0gmavsmxkz0h03-go-golang-org-x-tools-0.25.0))

->

(/gnu/store/7p6zk3zka35g3699b9kfl0njzwykimjm-go-github-com-stretchr-testify-1.10.0
 /gnu/store/ax54x3d7fyywbppqvf0gmavsmxkz0h03-go-golang-org-x-tools-0.25.0)

Sources installed with go-build-system and go-module-build-system have
a /src directory. Packages realistically have dozens of inputs (the go
compiler, coreutils, etc.) so this filtering is very much necessary."
  (map cdr
       (filter
        (lambda (input)
          (and (string-prefix? "go-" (car input))
               (directory-exists? (string-append (cdr input) "/src"))))
        inputs)))

(define* (infer-module-path-from-dir dir #:optional (subdir "src"))
  "Infer the go module 'github.com/user/module' from a store path.

DIR should be a a store path <store>/<pkg>

<store>/<pkg>/src/github.com/user/module/... -> github.com/user/module

By default, this function assumes the store path is a go-build-system
or go-module-build-system installed package with the sources installed
under DIR/SUBDIR."
  (with-directory-excursion (string-append dir "/" subdir)
    (string-trim
     (car
      (sort! (map dirname (find-files "." #:fail-on-error? #t))
             (lambda (x y) (< (string-length x) (string-length y)))))
     char-set:punctuation)))

(define* (module-path-from-file #:optional (go.mod "go.mod"))
  "Read the module-path from a go.mod file.

This assumes that go.mod exists and is well-defined. This can not be
assumed in general until after the build stage, in which case go.mod
should always have been generated in the root dir."
  (cadr (string-split (read-first-line go.mod) #\space)))

(define* (prepare-sources #:key module-path #:allow-other-keys)
  ;; Remove any go.mod and go.sum files. The builder will write its
  ;; own based on the build inputs, and any upstream checksums will be
  ;; wrong. This has to be done before setup-proxy as it would detect
  ;; the source dir's go.mod and go.sum files and fail because of
  ;; checksum mismatches. It is in its own phase so that it can be
  ;; overriden, if necessary.
  ;;
  ;; We want to init it with go init module so that it records the go
  ;; version in order to not fall back to too old go versions:
  ;; function instantiation requires go1.18 or later (-lang was set
  ;;   to go1.16; check go.mod)
  ;;
  ;; If the module path is explicitly set, use it. Otherwise,
  ;; infer it from the go.mod file. If the go.mod file does not
  ;; exist and module-path is not specified, fail the build.
(let ((module-path (or module-path (module-path-from-file))))
  (for-each delete-file (find-files "." "go\\.mod$"))
  (for-each delete-file (find-files "." "go\\.sum$"))
  (re-init-module module-path)))

(define* (setup-go-env #:key outputs cgo? environment-variables
                       goarch goos #:allow-other-keys)
  (let* ((go-proxy (string-append (getcwd) "/guix-go/proxy"))
         (go-cache (string-append (getcwd) "/guix-go/cache"))
         (go-mod-cache (string-append (getcwd) "/guix-go/modcache")))
    (setenv "GOSUMDB" "off")
    (setenv "GOPROXY" (string-append "file://" go-proxy))
    (setenv "GOCACHE" go-cache)
    (setenv "GOMODCACHE" go-mod-cache)
    (setenv "GOPATH" (string-append (assoc-ref outputs "out")))
    (when cgo?
      (setenv "CGO_ENABLED" "1"))

    (setenv "GOARCH" (or goarch (getenv "GOHOSTARCH")))
    (setenv "GOOS" (or goos (getenv "GOHOSTOS")))
    (match goarch
      ("arm"
       (setenv "GOARM" "7"))
      ((or "mips" "mipsel")
       (setenv "GOMIPS" "hardfloat"))
      ((or "mips64" "mips64le")
       (setenv "GOMIPS64" "hardfloat"))
      ((or "ppc64" "ppc64le")
       (setenv "GOPPC64" "power8"))
      (_ #t))

    (for-each
     (lambda (var) (setenv (car var) (cdr var)))
     environment-variables)))

(define (module-version-or-synthesized mod-input-path module-path)
  "Resolve module or synthesize module version.

Figure out what module version to use for go get to resolve, either
the package version or a special syntesized one.

This addresses a quirk of go module paths and the go module system. Go
expects that if a package has a version >= v2.x.y, the module path is
module/v2. When splitting a large package into smaller libraries that
share import prefix, the module path no longer ends with the major
version, and go get complains:

    invalid version: should be v0 or v1, not v4

The actual package versions used during the build matters little, and
is an implementation detail for the builder. For the packages with
version >= 2 with an \"unversioned\" module path, synthesize the special
version 0.0.1-guix.

MOD-INPUT-PATH should be the store path of the module as returned by
filter-go-inputs. MODULE-PATH should be the module path as it is
written in the go.mod, for example:

(module-version-or-synthesized
  \"/gnu/store/2v69cskzdjininks376wlw9cq3dv2gd1-go-github-com-stretchr-objx-0.5.2\"
  \"github.com/stretchr/objx\")
"
  (let* ((_ pkg-version (package-name->name+version
                         (strip-store-file-name mod-input-path)))
         (dir-version (basename module-path)))
    (if (and (string-match "^[2-9]+\\." pkg-version)
             (not (string-match "v[2-9]+$" dir-version)))
        "0.0.1-guix"
        pkg-version)))

(define* (setup-goproxy #:key inputs #:allow-other-keys)
  (let* (;; Remove file:// from the goproxy, we want the dir
         (go-proxy (substring (getenv "GOPROXY") 7)))
    (mkdir-p go-proxy)
    (for-each
     (lambda (mod-input-path)
       (let* ((store-path (string-append mod-input-path "/src"))
              (module-path (infer-module-path-from-dir mod-input-path))
              (source-dir (string-append store-path "/" module-path))
              (version (module-version-or-synthesized mod-input-path module-path))
              (module-dir (format #f "~a@v~a" module-path version))
              (proxy-dir (format #f "~a/~a/@v" go-proxy (go-path-escape module-path)))
              (proxy/mod (string-append proxy-dir "/v" version ".mod"))
              (proxy/zip (string-append proxy-dir "/v" version ".zip"))
              (tmp-dir "guix-tmp")
              (tmp-module (string-append tmp-dir "/" module-dir))
              (tmp-mod (string-append tmp-module "/go.mod")))

         ;; In some cases a module will show up twice, e.g. when
         ;; breaking cyclic dependencies. In that case, don't install
         ;; the second version. In that case the inputs have different
         ;; store paths, and we can't rely at all on the package name.
         (unless (file-exists? (string-append proxy-dir "/v" version ".mod"))
           (mkdir-p tmp-module)
           (copy-recursively source-dir tmp-module #:log #f)

           ;; Delete all go.mod and go.sum files, and re-write the
           ;; root one without dependencies and toolchain directives.
           ;; Note that we cannot use re-init-module here. If there is
           ;; a go.mod in the project root, it would be detected by
           ;; re-init-module, and if it happened to contain a
           ;; toolchain directive it would infect this module here.
           (for-each delete-file (find-files tmp-module "go\\.mod$"))
           (for-each delete-file (find-files tmp-module "go\\.sum$"))
           (with-directory-excursion tmp-module
             (re-init-module module-path))
           (for-each
            (lambda (f) (utime f 0 0 0 0 AT_SYMLINK_NOFOLLOW))
            (find-files tmp-dir #:directories? #t))

           ;; We need the -D flag, because go mod fails hard on any path
           ;; that does not begin with $module@version, even if for
           ;; sub-prefixes of $module
           (mkdir-p proxy-dir)
           (with-directory-excursion tmp-dir
             (invoke "zip" "-r" "-q" "-o" "-D" "-X" proxy/zip "."))

           (copy-file tmp-mod proxy/mod)
           (delete-file-recursively tmp-dir)
           (call-with-output-file (string-append proxy-dir "/v" version ".info")
             (lambda (f) (format f "{~s:\"v~a\"}~%" "Version" version)))

           (call-with-append-file (string-append proxy-dir "/list")
                                  (lambda (f) (format f "v~a~%" version))))))
     (filter-go-inputs inputs))))

;; These paths must be consistent across different stages, so use
;; symbols for them to ensure they're consistent
(define guix-install-cache "guix-out-cache")
(define var-build-cache "/var/cache/go/build")

(define* (setup-gocache #:key inputs #:allow-other-keys)
  (define (search-input-directories dir go-inputs)
    (filter directory-exists?
            (map (lambda (store) (string-append store "/" dir))
                 go-inputs)))

  (union-build (getenv "GOCACHE")
               (search-input-directories var-build-cache
                                         (filter-go-inputs inputs))
               ;; Creating all directories isn't that bad, because
               ;; there are only ever 256 of them.
               #:create-all-directories? #t
               #:log-port (%make-void-port "w")))

(define* (build #:key inputs go-flags tags build-targets
                install-targets trimpath? build-output-dir? skip-build?
                install-cache? install-source? parallel-build?
                #:allow-other-keys)
  (setenv "GOMAXPROCS"
          (number->string
           (if parallel-build? (parallel-job-count) 1)))

  (for-each
   (lambda (store-path)
     (let* ((module (infer-module-path-from-dir store-path))
            (version (module-version-or-synthesized store-path module)))
       (invoke "go" "get" (string-append module "@v" version))))
   (filter-go-inputs inputs))
    ;; go.mod and go.sum have had their timestamps updated by go get,
    ;; which will be snapshotted in the build cache and break it. From
    ;; here on out these files should not need to change, so fix the
    ;; timestamp. The sum will only exist if there are any
    ;; dependencies.
  (utime "go.mod" 0 0 0 0)
  (when (file-exists? "go.sum")
    (utime "go.sum" 0 0 0 0))

  ;; If -o is used it must be the first flag to build. This flag is
  ;; necessary when there is a command (program) with the same name
  ;; as its directory, e.g. info/main.go instead of cmd/info.go
  (unless skip-build?
    (for-each
     (lambda target
       (apply invoke "go" "build"
              (append
               (if build-output-dir?
                   (list "-o" (string-append (or (getenv "TMP") "/tmp")
                                             "/go-build/")) '())
               go-flags (make-tags tags)
               (if trimpath? '("-trimpath") '())
               target)))
     (append build-targets install-targets)))

  ;; Snapshot the cache before running tests. It is not interesting
  ;; to snapshot test artifacts, and they may pollute the cache with
  ;; non-reproducible artifacts.
  (when (and install-cache? (not skip-build?))
    (mkdir-p guix-install-cache)
    (with-directory-excursion guix-install-cache
      (copy-nonlink-recursively (getenv "GOCACHE") ".")
      (delete-file "trim.txt")
      (delete-file "README")
      (for-each set-cache-action-epoch (find-files "." "-a$")))))

(define* (check #:key inputs tests? go-flags tags test-flags
                test-targets module-path parallel-tests? trimpath?
                #:allow-other-keys)
  (when tests?
    (let ((njobs (if parallel-tests? (parallel-job-count) 1)))
      (setenv "GOMAXPROCS" (number->string njobs))
      (for-each
       (lambda target
         (apply invoke (append
                        (list "go" "test") go-flags (make-tags tags)
                        (if trimpath? '("-trimpath") '())
                        test-flags target)))
       test-targets))))

(define* (install #:key source inputs outputs target go-flags tags
                  install-targets install-cache? install-source?
                  trimpath? #:allow-other-keys)
  (for-each
   (lambda target
     (apply invoke (append (list "go" "install") go-flags (make-tags tags)
                           (if trimpath? '("-trimpath") '())
                           target)))
   install-targets)

  ;; When cross-compiling, binaries are installed into /bin/<arch>, so we need
  ;; to move them up to /bin. However, when *not* cross-compiling they end up
  ;; in /bin even when GOOS and GOARCH are set.
  (when target
    (let* ((bin (string-append (assoc-ref outputs "out") "/bin/"))
           (platform (string-append (getenv "GOOS") "_" (getenv "GOARCH")))
           (arch-dir (string-append bin platform)))
      (when (directory-exists? arch-dir)
        (copy-recursively arch-dir bin)
        (delete-file-recursively arch-dir))))

  (when (directory-exists? guix-install-cache)
    (with-directory-excursion guix-install-cache
      (copy-recursively
       "." (string-append (assoc-ref outputs "out") var-build-cache)
       #:log #f)))

  (when install-source?
    (let* ((out (assoc-ref outputs "out"))
           (name version (package-name->name+version (assoc-ref outputs "out")))
           (module-line (read-first-line "go.mod"))
           (module-path (cadr (string-split module-line #\space)))
           (dst (format #f "~a/src/~a" out module-path)))
      (copy-recursively source dst #:log #f)
      (when (file-exists? (string-append dst "/go.mod"))
        (make-file-writable (string-append dst "/go.mod")))
      (install-file "go.mod" dst))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (add-after 'unpack 'prepare-sources prepare-sources)
    (add-before 'build 'setup-go-env setup-go-env)
    (add-after 'setup-go-env 'setup-goproxy setup-goproxy)
    (add-after 'setup-goproxy 'setup-gocache setup-gocache)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (go-module-build #:key inputs (phases %standard-phases)
                          #:allow-other-keys #:rest args)
  "Go Module Build System"
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; go-module-build-system.scm ends here
