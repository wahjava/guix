(define-module (gnu packages forgejo)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages prometheus)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-github-com-checkpoint-restore-go-criu
  (package
    (name "go-github-com-checkpoint-restore-go-criu")
    (version "7.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/go-criu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07qnlmvdzm7gl3lf2kldm83n2bfqnh6qqlhi43734q0dk5bfhpym"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/go-criu/v7"
      #:unpack-path "github.com/checkpoint-restore/go-criu"))
    (propagated-inputs (list go-google-golang-org-protobuf go-golang-org-x-sys
                             go-github-com-spf13-cobra))
    (home-page "https://github.com/checkpoint-restore/go-criu")
    (synopsis "go-criu -- Go bindings for CRIU")
    (description
     "This repository provides Go bindings for @@url{https://criu.org/,CRIU}.  The
code is based on the Go-based PHaul implementation from the CRIU repository.
For easier inclusion into other Go projects, the CRIU Go bindings have been
moved to this repository.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-imgcrypt
  (package
    (name "go-github-com-containerd-imgcrypt")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/imgcrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sa0jijbxw24hgxmzkril0skzf5b3kihh8hqkfvm22l7v9c9qg17"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/imgcrypt/v2"
      #:unpack-path "github.com/containerd/imgcrypt"))
    (propagated-inputs (list go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-containers-ocicrypt
                             go-github-com-containerd-typeurl-v2
                             go-github-com-containerd-platforms
                             go-github-com-containerd-errdefs
                             go-github-com-containerd-containerd-v2))
    (home-page "https://github.com/containerd/imgcrypt")
    (synopsis "imgcrypt image encryption library and command line tool")
    (description
     "Project @@code{imgcrypt} is a non-core subproject of containerd.")
    (license license:asl2.0)))

(define-public go-github-com-mistifyio-go-zfs
  (package
    (name "go-github-com-mistifyio-go-zfs")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mistifyio/go-zfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19wasizw95x1dsc5wxqsxc3knbnkbk3xpha6l506dc5a7wjfs8ir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mistifyio/go-zfs/v3"
      #:unpack-path "github.com/mistifyio/go-zfs"))
    (propagated-inputs (list go-github-com-google-uuid))
    (home-page "https://github.com/mistifyio/go-zfs")
    (synopsis "Go Wrapper for ZFS")
    (description
     "Package zfs provides wrappers around the ZFS command line tools.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-zfs
  (package
    (name "go-github-com-containerd-zfs")
    (version "2.0.0-rc.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/zfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0448z4k61kp3dv97fj8sf8ghgb2zrwfpikb22izh8qns2ixj9pzy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/zfs/v2"
      #:unpack-path "github.com/containerd/zfs"))
    (propagated-inputs (list go-github-com-mistifyio-go-zfs-v3
                             go-github-com-containerd-plugin
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2))
    (home-page "https://github.com/containerd/zfs")
    (synopsis "ZFS snapshotter plugin")
    (description "ZFS snapshotter plugin for containerd.")
    (license license:asl2.0)))

(define-public go-github-com-tchap-go-patricia
  (package
    (name "go-github-com-tchap-go-patricia")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tchap/go-patricia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jri24s81bswsqp7idniz41qba52131g156b5br5z4ignqx8s5x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tchap/go-patricia/v2"
      #:unpack-path "github.com/tchap/go-patricia"))
    (home-page "https://github.com/tchap/go-patricia")
    (synopsis "go-patricia")
    (description
     "This package provides a generic patricia trie (also called radix tree)
implemented in Go (Golang).")
    (license license:expat)))

(define-public go-github-com-containerd-containerd-v2
  (package
    (name "go-github-com-containerd-containerd")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hi20jmj20kjgxf4zblx8ykwiyph0f8gslid920hj4nwsvdma59z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/containerd/v2"
      #:unpack-path "github.com/containerd/containerd"))
    (propagated-inputs (list go-tags-cncf-io-container-device-interface
                        go-k8s-io-utils
                        go-k8s-io-kubelet
                        go-k8s-io-klog-v2
                        go-k8s-io-cri-api
                        go-k8s-io-client-go
                        go-k8s-io-apimachinery
                        go-gopkg-in-inf-v0
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-mod
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-bbolt
                        go-github-com-vishvananda-netns
                        go-github-com-vishvananda-netlink
                        go-github-com-urfave-cli-v2
                        go-github-com-tchap-go-patricia-v2
                        go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-prometheus-client-golang
                        go-github-com-pelletier-go-toml-v2
                        go-github-com-opencontainers-selinux
                        go-github-com-opencontainers-runtime-tools
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-moby-sys-userns
                        go-github-com-moby-sys-user
                        go-github-com-moby-sys-symlink
                        go-github-com-moby-sys-signal
                        go-github-com-moby-sys-sequential
                        go-github-com-moby-sys-mountinfo
                        go-github-com-moby-locker
                        go-github-com-mdlayher-vsock
                        go-github-com-klauspost-compress
                        go-github-com-intel-goresctrl
                        go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-fsnotify-fsnotify
                        go-github-com-docker-go-units
                        go-github-com-docker-go-metrics
                        go-github-com-docker-go-events
                        go-github-com-distribution-reference
                        go-github-com-davecgh-go-spew
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-containernetworking-plugins
                        go-github-com-containernetworking-cni
                        go-github-com-containerd-zfs-v2
                        go-github-com-containerd-typeurl-v2
                        go-github-com-containerd-ttrpc
                        go-github-com-containerd-plugin
                        go-github-com-containerd-platforms
                        go-github-com-containerd-otelttrpc
                        go-github-com-containerd-nri
                        go-github-com-containerd-log
                        go-github-com-containerd-imgcrypt-v2
                        go-github-com-containerd-go-runc
                        go-github-com-containerd-go-cni
                        go-github-com-containerd-fifo
                        go-github-com-containerd-errdefs-pkg
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-api
                        go-github-com-containerd-console
                        go-github-com-containerd-cgroups-v3
                        go-github-com-containerd-btrfs-v2
                        go-github-com-checkpoint-restore-go-criu-v7
                        go-github-com-checkpoint-restore-checkpointctl
                        go-github-com-microsoft-hcsshim
                        go-github-com-microsoft-go-winio
                        go-github-com-adalogics-go-fuzz-headers
                        go-dario-cat-mergo))
    (home-page "https://github.com/containerd/containerd")
    (synopsis "Announcements")
    (description
     "containerd is an industry-standard container runtime with an emphasis on
simplicity, robustness, and portability.  It is available as a daemon for Linux
and Windows, which can manage the complete container lifecycle of its host
system: image transfer and storage, container execution and supervision,
low-level storage and network attachments, etc.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-continuity
  (package
    (name "go-github-com-containerd-continuity")
    (version "0.4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/continuity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01p5cqc0lvv6z5m0w23xq38fmc86k490wvylng5sfn90zplgjrwi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/continuity"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-opencontainers-go-digest
                             go-github-com-containerd-log
                             go-github-com-microsoft-go-winio))
    (home-page "https://github.com/containerd/continuity")
    (synopsis "continuity")
    (description
     "This package provides a transport-agnostic, filesystem metadata manifest system.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-fuse-overlayfs-snapshotter-v2
  (package
    (name "go-github-com-containerd-fuse-overlayfs-snapshotter")
    (version "2.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/fuse-overlayfs-snapshotter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "002mrbqmm8jzkfwbmrxlx5f4l843dpizc86zl0bchk9kyiygq8fz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/fuse-overlayfs-snapshotter/v2"
      #:unpack-path "github.com/containerd/fuse-overlayfs-snapshotter"))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-github-com-coreos-go-systemd-v22
                             go-github-com-containerd-plugin
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2
                             go-github-com-containerd-containerd-api))
    (home-page "https://github.com/containerd/fuse-overlayfs-snapshotter")
    (synopsis "snapshotter plugin for")
    (description
     "Unlike @@code{overlayfs}, @@code{fuse-overlayfs} can be used as a non-root user
on almost all recent distros.")
    (license license:asl2.0)))

(define-public go-github-com-mitchellh-hashstructure-v2
  (package
    (name "go-github-com-mitchellh-hashstructure")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/hashstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyr1igvyv7dzjxs9hbwk7qhshwxys0hq59sy2g2a46hjgi311iv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/hashstructure/v2"
      #:unpack-path "github.com/mitchellh/hashstructure"))
    (home-page "https://github.com/mitchellh/hashstructure")
    (synopsis "hashstructure")
    (description
     "hashstructure is a Go library for creating a unique hash value for arbitrary
values in Go.")
    (license license:expat)))

(define-public go-github-com-spdx-tools-golang
  (package
    (name "go-github-com-spdx-tools-golang")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spdx/tools-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pdcwb6hvv5zr5k844knpcgbhnm0zgpwaacd5cjahb196q6xpgbw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spdx/tools-golang"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-github-com-stretchr-testify
                             go-github-com-spdx-gordf
                             go-github-com-google-go-cmp
                             go-github-com-anchore-go-struct-converter))
    (home-page "https://github.com/spdx/tools-golang")
    (synopsis "SPDX tools-golang")
    (description
     "@@code{tools-golang} is a collection of Go packages intended to make it easier
for Go programs to work with @@url{https://spdx.dev/,SPDX®} files.")
    (license (list license:asl2.0 license:cc-by4.0))))

(define-public go-github-com-alecaivazis-survey-v2
  (package
    (name "go-github-com-alecaivazis-survey")
    (version "2.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlecAivazis/survey")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l3wqphqvm0qxv33pj9f1r72z5fln99vg735fcigv8k513m2aw9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alecaivazis/survey/v2"
      #:unpack-path "github.com/alecaivazis/survey"))
    (propagated-inputs (list go-golang-org-x-text
                             go-golang-org-x-term
                             go-github-com-stretchr-testify
                             go-github-com-mgutz-ansi
                             go-github-com-mattn-go-isatty
                             go-github-com-kballard-go-shellquote
                             go-github-com-hinshun-vt10x
                             go-github-com-creack-pty
                             go-github-com-netflix-go-expect))
    (home-page "https://github.com/alecaivazis/survey")
    (synopsis "Survey")
    (description
     "This package provides a library for building interactive and accessible prompts
on terminals supporting ANSI escape sequences.")
    (license license:expat)))

(define-public go-github-com-docker-cli
  (package
    (name "go-github-com-docker-cli")
    (version "28.2.2+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/cli")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qh5nm6kz1phj1wmh0misxzx4cbfzp8r51fkngh8ra01gzh8d8k5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/cli"))
    (home-page "https://github.com/docker/cli")
    (synopsis "Docker CLI")
    (description "This repository is the home of the Docker CLI.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v11
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "11.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "192d9x0lw73i1px2wn8b5wvb3gjhirrvaghpv15qamxhc5rmqd33"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v11"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v12
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "12.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15n81idz555rvfj8jr2sz18x2s9smvc8x2yj2x97djd9qn8b7b6a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v12"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v13
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "13.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07mh0yx0sz5r56rwx30nqcdnlvn9glqgbz0i6234flnqirybc92m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v13"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v14
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "14.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n4jzycy0z5iczvv018x6dvkh2nd47vijpx2a669bfdlybxan1cq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v14"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v15
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "15.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bw6083f47047y4m8pz76kaiiwwx8732ffvfi7bj1w0yvvf23ha7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v15"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-roaringbitmap-roaring-v2
  (package
    (name "go-github-com-roaringbitmap-roaring")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RoaringBitmap/roaring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10iiq0cvqdcz2fgklr63djp9zvvrx603wjjvzg3fs72rn6v108n3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/RoaringBitmap/roaring/v2"
      #:unpack-path "github.com/RoaringBitmap/roaring"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-mschoch-smat
                             go-github-com-google-uuid
                             go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/RoaringBitmap/roaring")
    (synopsis "roaring")
    (description
     "Package roaring is an implementation of Roaring Bitmaps in Go.  They provide
fast compressed bitmap data structures (also called bitset).  They are ideally
suited to represent sets of integers over relatively small ranges.  See
@@url{http://roaringbitmap.org,http://roaringbitmap.org} for details.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-scorch-segment-api
  (package
    (name "go-github-com-blevesearch-scorch-segment-api")
    (version "2.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/scorch_segment_api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rhn4pnm0kwlg94l6abwrvyncwd8nws14q67fs98rcdbaf6m3h05"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/scorch_segment_api/v2"
      #:unpack-path "github.com/blevesearch/scorch_segment_api"))
    (propagated-inputs (list go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/scorch_segment_api")
    (synopsis "Scorch Segment API")
    (description "Scorch supports a pluggable Segment interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v16
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "16.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hf8cf0lirhsqlr2dgg5rcnb4p0riqq2b5iknchzhmz56zn693i6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/blevesearch/zapx/v16"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-go-faiss
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-bits-and-blooms-bitset
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-bleve-v2
  (package
    (name "go-github-com-blevesearch-bleve")
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/bleve")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10cf3b9zjyngkv1p6mzc53iip12z5326w0s7lc5rpf8ij8zh69s3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/blevesearch/bleve/v2"
      #:unpack-path "github.com/blevesearch/bleve"))
    (propagated-inputs (list go-golang-org-x-text
                             go-go-etcd-io-bbolt
                             go-github-com-spf13-cobra
                             go-github-com-golang-protobuf
                             go-github-com-couchbase-moss
                             go-github-com-blevesearch-zapx-v16
                             go-github-com-blevesearch-zapx-v15
                             go-github-com-blevesearch-zapx-v14
                             go-github-com-blevesearch-zapx-v13
                             go-github-com-blevesearch-zapx-v12
                             go-github-com-blevesearch-zapx-v11
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-upsidedown-store-api
                             go-github-com-blevesearch-stempel
                             go-github-com-blevesearch-snowballstem
                             go-github-com-blevesearch-snowball
                             go-github-com-blevesearch-segment
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-gtreap
                             go-github-com-blevesearch-goleveldb
                             go-github-com-blevesearch-go-porterstemmer
                             go-github-com-blevesearch-go-metrics
                             go-github-com-blevesearch-go-faiss
                             go-github-com-blevesearch-geo
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-bits-and-blooms-bitset
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/bleve")
    (synopsis "bleve")
    (description "Package bleve is a library for indexing and searching text.")
    (license license:asl2.0)))

 (define-public go-github-com-buildkite-terminal-to-html-v3
  (package
    (name "go-github-com-buildkite-terminal-to-html")
    (version "3.16.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buildkite/terminal-to-html")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q9mav18k8xjswwmrjdr106mn645q639i6ayqav984fwb64kv04y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/buildkite/terminal-to-html/v3"
      #:unpack-path "github.com/buildkite/terminal-to-html"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-urfave-cli-v2
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/buildkite/terminal-to-html")
    (synopsis "Usage")
    (description "Package terminal converts ANSI input to HTML output.")
    (license license:expat)))

(define-public go-github-com-djherbis-nio-v3
  (package
    (name "go-github-com-djherbis-nio")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/nio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06zd92m0p4hd6mkrp3ya043p4f9f1hhqwvcl69hxmdr1an39b699"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/djherbis/nio/v3"
      #:unpack-path "github.com/djherbis/nio"))
    (propagated-inputs (list go-github-com-djherbis-buffer))
    (home-page "https://github.com/djherbis/nio")
    (synopsis "nio")
    (description "Package nio provides a few buffered io primitives.")
    (license license:expat)))

(define-public go-github-com-editorconfig-editorconfig-core-go-v2
  (package
    (name "go-github-com-editorconfig-editorconfig-core-go")
    (version "2.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/editorconfig/editorconfig-core-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m8jjxj73l6hva5idpbawcm1f3jiyd2qpfj8n2h21w07virhlib2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/editorconfig/editorconfig-core-go/v2"
      #:unpack-path "github.com/editorconfig/editorconfig-core-go"))
    (propagated-inputs (list go-gopkg-in-ini-v1 go-golang-org-x-mod
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/editorconfig/editorconfig-core-go")
    (synopsis "Editorconfig Core Go")
    (description
     "This package provides a @@url{https://editorconfig.org/,Editorconfig} file
parser and manipulator for Go.")
    (license license:expat)))

(define-public go-github-com-go-enry-go-enry-v2
  (package
    (name "go-github-com-go-enry-go-enry")
    (version "2.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-enry/go-enry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zm82gywncx96nnhiymh1cxhplb1rgzgb7r3fmnky1fqg23nrsnv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-enry/go-enry/v2"
      #:unpack-path "github.com/go-enry/go-enry"))
    (propagated-inputs (list go-gopkg-in-yaml-v2
                             go-github-com-stretchr-testify
                             go-github-com-go-enry-go-oniguruma))
    (home-page "https://github.com/go-enry/go-enry")
    (synopsis "go-enry")
    (description "Package enry identifies programming languages.")
    (license license:asl2.0)))

(define-public go-github-com-clickhouse-clickhouse-go
  (package
    (name "go-github-com-clickhouse-clickhouse-go")
    (version "2.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ClickHouse/clickhouse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x38p1kl0iz4qva0iv03r2akslygbi33xvk3fg96zxs6n0sdlh8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ClickHouse/clickhouse-go/v2"
      #:unpack-path "github.com/ClickHouse/clickhouse-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-golang-org-x-net
                             go-go-opentelemetry-io-otel-trace
                             go-github-com-testcontainers-testcontainers-go
                             go-github-com-stretchr-testify
                             go-github-com-shopspring-decimal
                             go-github-com-paulmach-orb
                             go-github-com-mkevac-debugcharts
                             go-github-com-google-uuid
                             go-github-com-docker-go-units
                             go-github-com-docker-go-connections
                             go-github-com-docker-docker
                             go-github-com-andybalholm-brotli
                             go-github-com-clickhouse-clickhouse-go
                             go-github-com-clickhouse-ch-go))
    (home-page "https://github.com/ClickHouse/clickhouse-go")
    (synopsis "ClickHouse")
    (description
     "Golang SQL database client for @@url{https://clickhouse.com/,@code{ClickHouse}}.")
    (license license:asl2.0)))

(define-public go-github-com-go-testfixtures-testfixtures-v3
  (package
    (name "go-github-com-go-testfixtures-testfixtures")
    (version "3.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-testfixtures/testfixtures")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "020smlvia49a7cb04f69sc5jkyhnihr67jy3mh0dwv23lk20vhfg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/go-testfixtures/testfixtures/v3"
      #:unpack-path "github.com/go-testfixtures/testfixtures"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-spf13-pflag
                             go-github-com-mattn-go-sqlite3
                             go-github-com-lib-pq
                             go-github-com-joho-godotenv
                             go-github-com-jackc-pgx-v4
                             go-github-com-googleapis-go-sql-spanner
                             go-github-com-goccy-go-yaml
                             go-github-com-go-sql-driver-mysql
                             go-github-com-denisenkom-go-mssqldb
                             go-github-com-clickhouse-clickhouse-go-v2
                             go-cloud-google-com-go-spanner))
    (home-page "https://github.com/go-testfixtures/testfixtures")
    (synopsis "testfixtures")
    (description
     "Writing tests is hard, even more when you have to deal with an SQL database.
This package aims to make writing functional tests for web apps written in Go
easier.")
    (license license:expat)))

(define-public go-github-com-google-go-github-v52
  (package
    (name "go-github-com-google-go-github")
    (version "52.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00xkzaap5awwlmm67cpcgah1qfnjd65vzhwi2vszs9n17r4hp7dm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-github/v52"
      #:unpack-path "github.com/google/go-github"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-google-go-querystring
                             go-github-com-google-go-cmp
                             go-github-com-protonmail-go-crypto))
    (home-page "https://github.com/google/go-github")
    (synopsis "go-github")
    (description "go-github is a Go client library for accessing the
@@url{https://docs.github.com/en/rest,@code{GitHub} API v3}.")
    (license license:bsd-3)))

(define-public go-github-com-olivere-elastic-v7
  (package
    (name "go-github-com-olivere-elastic")
    (version "7.0.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olivere/elastic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wgb891qs4rgw1sfpwfnplp57g4w4sq3ci31h7121cx118hy1v17"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olivere/elastic/v7"
      #:unpack-path "github.com/olivere/elastic"))
    (propagated-inputs (list go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-github-com-smartystreets-go-aws-auth
                             go-github-com-pkg-errors
                             go-github-com-opentracing-opentracing-go
                             go-github-com-mailru-easyjson
                             go-github-com-google-go-cmp
                             go-github-com-fortytw2-leaktest
                             go-github-com-aws-aws-sdk-go))
    (home-page "https://github.com/olivere/elastic")
    (synopsis "Elastic")
    (description
     "Package elastic provides an interface to the Elasticsearch server
(@@url{https://www.elastic.co/products/elasticsearch,https://www.elastic.co/products/elasticsearch}).")
    (license license:expat)))

(define-public go-github-com-redis-go-redis-v9
  (package
    (name "go-github-com-redis-go-redis")
    (version "6.15.9+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y13zhv4isf28bq249pz9dp08rb8amyfp2gdbfah09zcmlhjsaki"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/redis/go-redis"))
    (home-page "https://github.com/redis/go-redis")
    (synopsis "Redis client for Golang")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-santhosh-tekuri-jsonschema-v5
  (package
    (name "go-github-com-santhosh-tekuri-jsonschema")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/santhosh-tekuri/jsonschema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vgcxi7dim1cjkr49ajcbmpsqjfn9srzxhd6zcvi3as1hdbqklsl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/santhosh-tekuri/jsonschema"))
    (home-page "https://github.com/santhosh-tekuri/jsonschema")
    (synopsis "jsonschema")
    (description
     "Package jsonschema provides json-schema compilation and validation.")
    (license license:bsd-3)))

(define-public go-connectrpc-com-connect
  (package
    (name "go-connectrpc-com-connect")
    (version "1.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/connectrpc/connect-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a6rzp57srhyf66jri62gfsj4ndpfxgb9ln15qdpfwv0xvcffz63"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "connectrpc.com/connect"))
    (propagated-inputs (list go-google-golang-org-protobuf go-golang-org-x-net
                             go-github-com-google-go-cmp))
    (home-page "https://connectrpc.com/connect")
    (synopsis "Connect")
    (description
     "Package connect is a slim RPC framework built on Protocol Buffers and
@@url{/net/http,net/http}.  In addition to supporting its own protocol, Connect
handlers and clients are wire-compatible with @code{gRPC} and @code{gRPC-Web},
including streaming.")
    (license license:asl2.0)))

(define-public go-code-gitea-io-actions-proto-go
  (package
    (name "go-code-gitea-io-actions-proto-go")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/gitea/actions-proto-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdrsr7kx20nhp1r54xyrq4gcwxvyzv636bzmsrchikffhq773b6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "code.gitea.io/actions-proto-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-connectrpc-com-connect))
    (home-page "https://code.gitea.io/actions-proto-go")
    (synopsis "proto-go")
    (description #f)
    (license license:expat)))

(define-public go-code-gitea-io-gitea-vet
  (package
    (name "go-code-gitea-io-gitea-vet")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/gitea/gitea-vet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gdq9r2y1mfyxbfq1dqhgy0kjxz29jn9xzpipay10scyi6miishv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "code.gitea.io/gitea-vet"))
    (propagated-inputs (list go-golang-org-x-tools))
    (home-page "https://code.gitea.io/gitea-vet")
    (synopsis "gitea-vet")
    (description "@@code{go vet} tool for Gitea.")
    (license license:expat)))

(define-public go-codeberg-org-gusted-mcaptcha
  (package
    (name "go-codeberg-org-gusted-mcaptcha")
    (version "0.0.0-20220723083913-4f3072e1d570")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/gusted/mcaptcha.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d1lq5bff099i923b3r63fwzi72j3qmkmsj1p7ns3vy3lg8v9p0d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "codeberg.org/gusted/mcaptcha"))
    (home-page "https://codeberg.org/gusted/mcaptcha")
    (synopsis "mCaptcha Go")
    (description "@code{mCaptcha} is a Go library to interact with
@@url{https://github.com/@code{mCaptcha/mCaptcha,mCaptcha}}.")
    (license license:expat)))

(define-public go-gitea-com-go-chi-binding
  (package
    (name "go-gitea-com-go-chi-binding")
    (version "0.0.0-20240430071103-39a851e106ed")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/go-chi/binding.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1755bhbhb9366fvdb59qy6f2riv20f7w9d104mkxr1cxjr0ifh52"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/go-chi/binding"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-goccy-go-json
                             go-github-com-go-chi-chi-v5))
    (home-page "https://gitea.com/go-chi/binding")
    (synopsis "License")
    (description
     "Package binding is a middleware that provides request data binding and
validation for Chi.")
    (license license:asl2.0)))

(define-public go-github-com-go-redis-redis
  (package
    (name "go-github-com-go-redis-redis")
    (version "6.15.9+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y13zhv4isf28bq249pz9dp08rb8amyfp2gdbfah09zcmlhjsaki"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-redis/redis"))
    (home-page "https://github.com/go-redis/redis")
    (synopsis "Redis client for Golang")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-lunny-nodb
  (package
    (name "go-github-com-lunny-nodb")
    (version "0.0.0-20160621015157-fc1ef06ad4af")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lunny/nodb")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08p6sxphi5w9mm43pj9ma5v4n5r2v0xr7nzmp2nya3hpn9fq2vcj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lunny/nodb"))
    (home-page "https://github.com/lunny/nodb")
    (synopsis "NoDB")
    (description "package nodb is a high performance embedded @code{NoSQL}.")
    (license license:expat)))

(define-public go-gitea-com-go-chi-cache
  (package
    (name "go-gitea-com-go-chi-cache")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/go-chi/cache.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12c7w6d9m7kplp2jszr453b18zazyw6rvhrcai9mfvj9lv35w1jh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/go-chi/cache"))
    (propagated-inputs (list go-gopkg-in-ini-v1
                             go-golang-org-x-crypto
                             go-github-com-unknwon-com
                             go-github-com-stretchr-testify
                             go-github-com-siddontang-ledisdb
                             go-github-com-lunny-nodb
                             go-github-com-lib-pq
                             go-github-com-go-sql-driver-mysql
                             go-github-com-go-redis-redis
                             go-github-com-bradfitz-gomemcache))
    (home-page "https://gitea.com/go-chi/cache")
    (synopsis "cache")
    (description
     "Package cache is a middleware that aim to have a transparent interface for a lot
of cache implementations.")
    (license license:asl2.0)))

(define-public go-gitea-com-go-chi-captcha
  (package
    (name "go-gitea-com-go-chi-captcha")
    (version "0.0.0-20240315150714-fb487f629098")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/go-chi/captcha.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15417ds7h7xqa1xy02g83qk8by3labx78zsvjjgbmdml1rxwkvwg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/go-chi/captcha"))
    (propagated-inputs (list go-github-com-unknwon-com
                             go-github-com-smartystreets-goconvey
                             go-github-com-go-chi-chi-v5
                             go-gitea-com-go-chi-cache))
    (home-page "https://gitea.com/go-chi/captcha")
    (synopsis "captcha")
    (description
     "Package captcha a middleware that provides captcha service for chi.")
    (license license:asl2.0)))

(define-public go-github-com-couchbase-go-couchbase
  (package
    (name "go-github-com-couchbase-go-couchbase")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/couchbase/go-couchbase")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dpy8148a1fz47yibn8mzl29savqg4jkfvyp8vvgsi0zp7jmwj89"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/couchbase/go-couchbase"))
    (home-page "https://github.com/couchbase/go-couchbase")
    (synopsis "A smart client for couchbase in go")
    (description "Package couchbase provides a smart client for go.")
    (license license:expat)))

(define-public go-github-com-go-redis-redis
  (package
    (name "go-github-com-go-redis-redis")
    (version "8.11.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a126m4k8mjavxxyqwmhkyvh54sn113l85mx5zmjph6hlnwzn9cm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-redis/redis/v8"
      #:unpack-path "github.com/go-redis/redis"))
    (propagated-inputs (list go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo
                             go-github-com-dgryski-go-rendezvous
                             go-github-com-cespare-xxhash-v2))
    (home-page "https://github.com/go-redis/redis")
    (synopsis "Redis client for Go")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-glendc-gopher-json
  (package
    (name "go-github-com-glendc-gopher-json")
    (version "0.0.0-20170414221815-dc4743023d0c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GlenDC/gopher-json")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hvam978ls0768smwfywwfg2dy816bfifch4hdwwbsx2d59zpphs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/glendc/gopher-json"))
    (home-page "https://github.com/glendc/gopher-json")
    (synopsis "gopher-json")
    (description
     "Package json is a simple JSON encoder/decoder for gopher-lua.")
    (license license:unlicense)))

(define-public go-github-com-siddontang-go
  (package
    (name "go-github-com-siddontang-go")
    (version "0.0.0-20180604090527-bdc77568d726")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/siddontang/go")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qx28xwqby3pl2r62y700x7j7aplmfm4hrq0y49p4ar8927mpxl6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/siddontang/go"))
    (home-page "https://github.com/siddontang/go")
    (synopsis "golib")
    (description "my golang lib.")
    (license license:expat)))

(define-public go-github-com-siddontang-goredis
  (package
    (name "go-github-com-siddontang-goredis")
    (version "0.0.0-20180423163523-0b4019cbd7b7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/siddontang/goredis")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cmkmljgyqvfc5ba5jj6xfiwdc82vksagvh2v7z06265i2snvhw5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/siddontang/goredis"))
    (home-page "https://github.com/siddontang/goredis")
    (synopsis #f)
    (description "Package goredis is a client for the redis and ledisdb.")
    (license license:expat)))

(define-public go-github-com-siddontang-rdb
  (package
    (name "go-github-com-siddontang-rdb")
    (version "0.0.0-20150307021120-fc89ed2e418d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/siddontang/rdb")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107wb2kcg67iggfx1bjmm5nhy8cg96zi1iw7nkv9dydivnvalbbd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/siddontang/rdb"))
    (home-page "https://github.com/siddontang/rdb")
    (synopsis "rdb")
    (description "Handling Redis RDB format.")
    (license license:expat)))

(define-public go-github-com-ugorji-go
  (package
    (name "go-github-com-ugorji-go")
    (version "1.2.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ugorji/go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mny5gm5gr82hz4y6k5ljaa0khjw647ys278wq750fgrbzp6fs8h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ugorji/go"))
    (propagated-inputs (list go-github-com-ugorji-go-codec))
    (home-page "https://github.com/ugorji/go")
    (synopsis "go-codec")
    (description
     "This repository contains the @@code{go-codec} library, the @@code{codecgen} tool
and benchmarks for comparing against other libraries.")
    (license license:expat)))

(define-public go-github-com-siddontang-ledisdb
  (package
    (name "go-github-com-siddontang-ledisdb")
    (version "0.0.0-20200510135210-d35789ec47e6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledisdb/ledisdb")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vjsjscnbg2l9id5psn3ja0hs0jf3bal01b87cx34swjxmnawh1p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/siddontang/ledisdb"))
    (propagated-inputs (list go-golang-org-x-net
                             go-github-com-yuin-gopher-lua
                             go-github-com-ugorji-go
                             go-github-com-syndtr-goleveldb
                             go-github-com-siddontang-rdb
                             go-github-com-siddontang-goredis
                             go-github-com-siddontang-go
                             go-github-com-peterh-liner
                             go-github-com-pelletier-go-toml
                             go-github-com-glendc-gopher-json
                             go-github-com-edsrzf-mmap-go))
    (home-page "https://github.com/siddontang/ledisdb")
    (synopsis "LedisDB")
    (description
     "Ledisdb is a high-performance @code{NoSQL} database, similar to Redis, written
in @@url{http://golang.org/,Go}.  It supports many data structures including kv,
list, hash, zset, set.")
    (license license:expat)))

(define-public go-github-com-unknwon-com
  (package
    (name "go-github-com-unknwon-com")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/unknwon/com")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02k1539gwcgp2bsjhy1mcm5y3fvsfbh2707ch70dyjqy7ibifdav"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/unknwon/com"))
    (propagated-inputs (list go-github-com-smartystreets-goconvey))
    (home-page "https://github.com/unknwon/com")
    (synopsis "Common Functions")
    (description
     "Package com is an open source project for commonly used functions for the Go
programming language.")
    (license license:asl2.0)))

(define-public go-gitea-com-go-chi-session
  (package
    (name "go-gitea-com-go-chi-session")
    (version "0.0.0-20240316035857-16768d98ec96")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/go-chi/session.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yx224mw1q2halp4h4ac0k3wacalnmn1pccjw8kilz1x06vf60n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/go-chi/session"))
    (propagated-inputs (list go-gopkg-in-ini-v1
                             go-github-com-unknwon-com
                             go-github-com-smartystreets-goconvey
                             go-github-com-siddontang-ledisdb
                             go-github-com-lib-pq
                             go-github-com-go-sql-driver-mysql
                             go-github-com-go-redis-redis-v8
                             go-github-com-go-chi-chi-v5
                             go-github-com-couchbase-go-couchbase
                             go-github-com-bradfitz-gomemcache))
    (home-page "https://gitea.com/go-chi/session")
    (synopsis "Session")
    (description
     "Package session a middleware that provides the session management of Macaron.")
    (license license:asl2.0)))

(define-public go-gitea-com-lunny-dingtalk-webhook
  (package
    (name "go-gitea-com-lunny-dingtalk-webhook")
    (version "0.0.0-20171025031554-e3534c89ef96")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/lunny/dingtalk_webhook.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dw6vzv6aq1yfxyllc406q69vlrk39m5jdcj355y9h9ak84plznw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/lunny/dingtalk_webhook"))
    (home-page "https://gitea.com/lunny/dingtalk_webhook")
    (synopsis "非官方 Dingtalk webhook Golang SDK")
    (description "首先在dingtalk中创建一个机器人，将@code{accessToken拷贝出来，然后执行下面方法即可}.")
    (license license:expat)))

(define-public go-gitea-com-lunny-levelqueue
  (package
    (name "go-gitea-com-lunny-levelqueue")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/lunny/levelqueue.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ppgr841qp7z2gp93ymkw7xy56jdb6h2r9zy8nn4v3d3lxr0gl0n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/lunny/levelqueue"))
    (propagated-inputs (list go-github-com-syndtr-goleveldb
                             go-github-com-stretchr-testify))
    (home-page "https://gitea.com/lunny/levelqueue")
    (synopsis "levelqueue")
    (description
     "Level queue is a simple queue golang library base on go-leveldb.")
    (license license:expat)))

(define-public go-github-com-42wim-sshsig
  (package
    (name "go-github-com-42wim-sshsig")
    (version "0.0.0-20250502153856-5100632e8920")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/42wim/sshsig")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14aw91jlk13vww4cdk5xcwg27y463y0b83sfnjhpw2c62rinb5mh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/42wim/sshsig"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/42wim/sshsig")
    (synopsis "Armored ssh signatures in go")
    (description
     "Package sshsig implements signing/verifying armored SSH signatures.  You can use
this package to sign data and verify signatures using your ssh private keys or
your ssh agent.  It gives the same output as using `ssh-keygen`, eg when signing
`ssh-keygen -Y sign -f keyfile -n namespace data`.")
    (license license:asl2.0)))

(define-public go-github-com-blakesmith-ar
  (package
    (name "go-github-com-blakesmith-ar")
    (version "0.0.0-20190502131153-809d4375e1fb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blakesmith/ar")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00fxkc04b9cb53xxzw5gdqqpwlqv9n5kk0yn2lb5w4rgj5gm8ph1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blakesmith/ar"))
    (home-page "https://github.com/blakesmith/ar")
    (synopsis "Golang ar (archive) file reader")
    (description "Copyright (c) 2013 Blake Smith <blakesmith0@@gmail.com>.")
    (license license:expat)))

(define-public go-github-com-bufbuild-connect-go
  (package
    (name "go-github-com-bufbuild-connect-go")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bufbuild/connect-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04j1k1fdz5jp45349km65x9smhcb5vp6hy5dn72jafn4yc0xxkbi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bufbuild/connect-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/bufbuild/connect-go")
    (synopsis "Connect")
    (description
     "Package connect is a slim RPC framework built on Protocol Buffers and
@@url{/net/http,net/http}.  In addition to supporting its own protocol, Connect
handlers and clients are wire-compatible with @code{gRPC} and @code{gRPC-Web},
including streaming.")
    (license license:asl2.0)))

;; (define-public go-github-com-buildkite-terminal-to-html
;;   (package
;;     (name "go-github-com-buildkite-terminal-to-html")
;;     (version "3.16.8")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/buildkite/terminal-to-html")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0q9mav18k8xjswwmrjdr106mn645q639i6ayqav984fwb64kv04y"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:go go-1.23
;;       #:import-path "github.com/buildkite/terminal-to-html/v3"
;;       #:unpack-path "github.com/buildkite/terminal-to-html"))
;;     (propagated-inputs (list go-golang-org-x-sys go-github-com-urfave-cli-v2
;;                              go-github-com-google-go-cmp))
;;     (home-page "https://github.com/buildkite/terminal-to-html")
;;     (synopsis "Usage")
;;     (description "Package terminal converts ANSI input to HTML output.")
;;     (license license:expat)))

(define-public go-github-com-chi-middleware-proxy
  (package
    (name "go-github-com-chi-middleware-proxy")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chi-middleware/proxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pslb4x8jhblgg7sfahrsiv7r4ay5aizgrqkrpfpwzsnhw88fl6h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/chi-middleware/proxy"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-go-chi-chi-v5))
    (home-page "https://github.com/chi-middleware/proxy")
    (synopsis "proxy middleware")
    (description
     "Forwarded headers middleware to use if application is run behind reverse proxy.")
    (license license:expat)))

(define-public go-github-com-dimiro1-reply
  (package
    (name "go-github-com-dimiro1-reply")
    (version "0.0.0-20200315094148-d0136a4c9e21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimiro1/reply")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b0g7zlsv3f59x19w0fvrp476fd36a4w4gpvafrmj04rwsb592jr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dimiro1/reply"))
    (propagated-inputs (list go-github-com-dlclark-regexp2))
    (home-page "https://github.com/dimiro1/reply")
    (synopsis "reply")
    (description
     "Package reply package is essentially a source code conversion of the ruby
library
@@url{https://github.com/discourse/email_reply_trimmer,https://github.com/discourse/email_reply_trimmer}.
 The core logic is a almost line by line conversion.")
    (license license:expat)))

(define-public go-github-com-djherbis-buffer
  (package
    (name "go-github-com-djherbis-buffer")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/buffer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17m6la583p9yskcj3bmhnazj8j4v8bmfjjp0kkv8i0zhqmcm0wmq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/djherbis/buffer"))
    (home-page "https://github.com/djherbis/buffer")
    (synopsis "Buffer")
    (description
     "Package buffer implements a series of Buffers which can be composed to implement
complicated buffering strategies.")
    (license license:expat)))

(define-public go-github-com-djherbis-nio
  (package
    (name "go-github-com-djherbis-nio")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/nio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06zd92m0p4hd6mkrp3ya043p4f9f1hhqwvcl69hxmdr1an39b699"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/djherbis/nio/v3"
      #:unpack-path "github.com/djherbis/nio"))
    (propagated-inputs (list go-github-com-djherbis-buffer))
    (home-page "https://github.com/djherbis/nio")
    (synopsis "nio")
    (description "Package nio provides a few buffered io primitives.")
    (license license:expat)))

;; (define-public go-github-com-editorconfig-editorconfig-core-go
;;   (package
;;     (name "go-github-com-editorconfig-editorconfig-core-go")
;;     (version "2.6.3")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/editorconfig/editorconfig-core-go")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0m8jjxj73l6hva5idpbawcm1f3jiyd2qpfj8n2h21w07virhlib2"))))
;;     (build-system go-build-system)
;;     (arguments
;;      (list
;;       #:go go-1.23
;;       #:import-path "github.com/editorconfig/editorconfig-core-go/v2"
;;       #:unpack-path "github.com/editorconfig/editorconfig-core-go"))
;;     (propagated-inputs (list go-gopkg-in-ini-v1 go-golang-org-x-mod
;;                              go-github-com-google-go-cmp))
;;     (home-page "https://github.com/editorconfig/editorconfig-core-go")
;;     (synopsis "Editorconfig Core Go")
;;     (description
;;      "This package provides a @@url{https://editorconfig.org/,Editorconfig} file
;; parser and manipulator for Go.")
;;     (license license:expat)))

(define-public go-github-com-blevesearch-geo
  (package
    (name "go-github-com-blevesearch-geo")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/geo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04c1zq5y4b283kmdd9yd89gdkz6xr7dy0v1hnlafrydlm389xdx5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/geo"))
    (propagated-inputs (list go-github-com-json-iterator-go
                             go-github-com-google-go-cmp
                             go-github-com-blevesearch-bleve-index-api))
    (home-page "https://github.com/blevesearch/geo")
    (synopsis "S2 geometry library in Go")
    (description
     "This forked version of s2geometry implements a few essential functionalities
like region term indexer, polygon projections etc for enabling the use of s2geo
in the information retrieval systems that uses an inverted index for indexing
and querying the spatial tokens.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-go-metrics
  (package
    (name "go-github-com-blevesearch-go-metrics")
    (version "0.0.0-20201227073835-cf1acfcdf475")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/go-metrics")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7zab04slz07c7l4h2cqz62qnqah69r6p157vvbd7725a7wzkr0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/go-metrics"))
    (home-page "https://github.com/blevesearch/go-metrics")
    (synopsis "go-metrics")
    (description "Go port of Coda Hale's Metrics library.")
    (license license:bsd-2)))

(define-public go-github-com-blevesearch-go-porterstemmer
  (package
    (name "go-github-com-blevesearch-go-porterstemmer")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/go-porterstemmer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nj448j7kj31vg76xa7nh2i6iz4b4fnvarh0dgsl11ay1pmfhj45"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/go-porterstemmer"))
    (home-page "https://github.com/blevesearch/go-porterstemmer")
    (synopsis "This fork...")
    (description
     "I'm maintaining this fork because the original author was not replying to issues
or pull requests.  For now I plan on maintaining this fork as necessary.")
    (license license:expat)))

(define-public go-github-com-blevesearch-goleveldb
  (package
    (name "go-github-com-blevesearch-goleveldb")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/goleveldb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yx4vp60nyn7y5i9nfg0vkm3w62lgbgn16la44xh0yjvacqqbm01"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/goleveldb"))
    (propagated-inputs (list go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo
                             go-github-com-golang-snappy))
    (home-page "https://github.com/blevesearch/goleveldb")
    (synopsis "Installation")
    (description
     "This is an implementation of the
@@url{http:code.google.com/p/leveldb,@code{LevelDB} key/value database} in the
@@url{http:golang.org,Go programming language}.")
    (license license:bsd-2)))

(define-public go-github-com-blevesearch-gtreap
  (package
    (name "go-github-com-blevesearch-gtreap")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/gtreap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pkcwgn2nkgqg01w95ivwbggcxihrc04k8i3wgaif2f437jpz2h7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/gtreap"))
    (home-page "https://github.com/blevesearch/gtreap")
    (synopsis "gtreap")
    (description
     "gtreap is an immutable treap implementation in the Go Language.")
    (license license:expat)))

(define-public go-github-com-blevesearch-segment
  (package
    (name "go-github-com-blevesearch-segment")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/segment")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y761p1m98kii5ccgfsc96wmlccj41dmg16pa4m0dj03dry130c9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/segment"))
    (home-page "https://github.com/blevesearch/segment")
    (synopsis "segment")
    (description
     "Package segment is a library for performing Unicode Text Segmentation as
described in Unicode Standard Annex #29
@@url{http://www.unicode.org/reports/tr29/,http://www.unicode.org/reports/tr29/}.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-snowball
  (package
    (name "go-github-com-blevesearch-snowball")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/snowball")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0infs0y0kps7hm255z8x8wq7cnns6c92iasb3k1z2vg80b9wgplf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/snowball"))
    (home-page "https://github.com/blevesearch/snowball")
    (synopsis "Snowball")
    (description
     "This package provides a @@url{http://golang.org,Go (golang)} implementation of
the @@url{http://snowball.tartarus.org/,Snowball stemmer} for natural language
processing.")
    (license license:expat)))

(define-public go-github-com-blevesearch-snowballstem
  (package
    (name "go-github-com-blevesearch-snowballstem")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/snowballstem")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yzglihjjn588xmmkaawqhc95pkk1cyc4bq7ipw7jqfw2np1f2rm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/snowballstem"))
    (home-page "https://github.com/blevesearch/snowballstem")
    (synopsis "snowballstem")
    (description
     "This repository contains the Go stemmers generated by the
@@url{https://github.com/snowballstem/snowball,Snowball} project.  They are
maintained outside of the core bleve package so that they may be more easily be
reused in other contexts.")
    (license license:bsd-3)))

(define-public go-github-com-blevesearch-stempel
  (package
    (name "go-github-com-blevesearch-stempel")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/stempel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0myqkpsr031q1ipw25lbd1ni1ygvwd50zirjgs7pj47q30hy70xa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/stempel"))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://github.com/blevesearch/stempel")
    (synopsis "stempel")
    (description
     "This package provides a Go implementation of the
@@url{http://www.getopt.org/stempel/,Stempel} stemmer, an algorithmic stemmer
frequently used with (but not limited to) the Polish language.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-upsidedown-store-api
  (package
    (name "go-github-com-blevesearch-upsidedown-store-api")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/upsidedown_store_api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sjs1ad4iprnmrac6g5acrb68g30q6zfihvnxx498iwq3sg4rbrz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/upsidedown_store_api"))
    (home-page "https://github.com/blevesearch/upsidedown_store_api")
    (synopsis "Upsidedown Store API")
    (description
     "Upsidedown supports a pluggable Key/Value storage interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "11.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "192d9x0lw73i1px2wn8b5wvb3gjhirrvaghpv15qamxhc5rmqd33"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v11"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "12.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15n81idz555rvfj8jr2sz18x2s9smvc8x2yj2x97djd9qn8b7b6a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v12"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "13.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07mh0yx0sz5r56rwx30nqcdnlvn9glqgbz0i6234flnqirybc92m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v13"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "14.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n4jzycy0z5iczvv018x6dvkh2nd47vijpx2a669bfdlybxan1cq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v14"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "15.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bw6083f47047y4m8pz76kaiiwwx8732ffvfi7bj1w0yvvf23ha7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/zapx/v15"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-go-faiss
  (package
    (name "go-github-com-blevesearch-go-faiss")
    (version "1.0.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/go-faiss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xnz8sjxq6bfg2rbm5dxsfb97hk0b88hg8z44a6ndnqplppvvjbd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/go-faiss"))
    (home-page "https://github.com/blevesearch/go-faiss")
    (synopsis "go-faiss")
    (description
     "Package faiss provides bindings to Faiss, a library for vector similarity
search.  More detailed documentation can be found at the Faiss wiki:
@@url{https://github.com/facebookresearch/faiss/wiki,https://github.com/facebookresearch/faiss/wiki}.")
    (license license:expat)))

(define-public go-github-com-roaringbitmap-roaring
  (package
    (name "go-github-com-roaringbitmap-roaring")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RoaringBitmap/roaring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10iiq0cvqdcz2fgklr63djp9zvvrx603wjjvzg3fs72rn6v108n3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/RoaringBitmap/roaring/v2"
      #:unpack-path "github.com/RoaringBitmap/roaring"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-mschoch-smat
                             go-github-com-google-uuid
                             go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/RoaringBitmap/roaring")
    (synopsis "roaring")
    (description
     "Package roaring is an implementation of Roaring Bitmaps in Go.  They provide
fast compressed bitmap data structures (also called bitset).  They are ideally
suited to represent sets of integers over relatively small ranges.  See
@@url{http://roaringbitmap.org,http://roaringbitmap.org} for details.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-scorch-segment-api
  (package
    (name "go-github-com-blevesearch-scorch-segment-api")
    (version "2.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/scorch_segment_api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rhn4pnm0kwlg94l6abwrvyncwd8nws14q67fs98rcdbaf6m3h05"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/scorch_segment_api/v2"
      #:unpack-path "github.com/blevesearch/scorch_segment_api"))
    (propagated-inputs (list go-github-com-blevesearch-bleve-index-api
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/scorch_segment_api")
    (synopsis "Scorch Segment API")
    (description "Scorch supports a pluggable Segment interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-vellum
  (package
    (name "go-github-com-blevesearch-vellum")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/vellum")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2rry2g51c3qjf467728nmcj579hi0rcs65lll9idy7fa9rn2ij"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/vellum"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-blevesearch-mmap-go
                             go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/blevesearch/vellum")
    (synopsis "vellum")
    (description
     "Package vellum is a library for building, serializing and executing an FST
(finite state transducer).")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx
  (package
    (name "go-github-com-blevesearch-zapx")
    (version "16.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/zapx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hf8cf0lirhsqlr2dgg5rcnb4p0riqq2b5iknchzhmz56zn693i6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/blevesearch/zapx/v16"
      #:unpack-path "github.com/blevesearch/zapx"))
    (propagated-inputs (list go-github-com-spf13-cobra
                             go-github-com-golang-snappy
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-mmap-go
                             go-github-com-blevesearch-go-faiss
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-bits-and-blooms-bitset
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @@url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the indepenent interface modules:.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-mmap-go
  (package
    (name "go-github-com-blevesearch-mmap-go")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/mmap-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y6qcx92zdcf2gg1ksrqydrgpcxm5x8i8zqlcaiq50ix04q2fbgk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/mmap-go"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/blevesearch/mmap-go")
    (synopsis "mmap-go")
    (description
     "Package mmap allows mapping files into memory.  It tries to provide a simple,
reasonably portable interface, but doesn't go out of its way to abstract away
every little platform detail.  This specifically means:.")
    (license license:bsd-3)))

(define-public go-github-com-couchbase-ghistogram
  (package
    (name "go-github-com-couchbase-ghistogram")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/couchbase/ghistogram")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05nhcp5i8l9ndcf18bn58qgm6vh10d59xnxz6qikk0sajyy4r2s1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/couchbase/ghistogram"))
    (home-page "https://github.com/couchbase/ghistogram")
    (synopsis "ghistogram")
    (description
     "Simple histogram for golang that avoids runtime memory allocations.")
    (license license:asl2.0)))

(define-public go-github-com-mschoch-smat
  (package
    (name "go-github-com-mschoch-smat")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mschoch/smat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qcb2jjg37krxmc915kqynghd6n26w2wxwgcafvxcwn8g0jx96qd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mschoch/smat"))
    (home-page "https://github.com/mschoch/smat")
    (synopsis "smat – State Machine Assisted Testing")
    (description
     "The concept is simple, describe valid uses of your library as states and
actions.  States describe which actions are possible, and with what probability
they should occur.  Actions mutate the context and transition to another state.")
    (license license:asl2.0)))

(define-public go-github-com-couchbase-moss
  (package
    (name "go-github-com-couchbase-moss")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/couchbase/moss")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wxy8d6116w5a3rwsjbkc9p8x1r2q011gxwx2ciqr2if4g59i601"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/couchbase/moss"))
    (propagated-inputs (list go-github-com-mschoch-smat
                             go-github-com-couchbase-ghistogram
                             go-github-com-blevesearch-mmap-go))
    (home-page "https://github.com/couchbase/moss")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-blevesearch-bleve
  (package
    (name "go-github-com-blevesearch-bleve")
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/bleve")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10cf3b9zjyngkv1p6mzc53iip12z5326w0s7lc5rpf8ij8zh69s3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/blevesearch/bleve/v2"
      #:unpack-path "github.com/blevesearch/bleve"))
    (propagated-inputs (list go-golang-org-x-text
                             go-go-etcd-io-bbolt
                             go-github-com-spf13-cobra
                             go-github-com-golang-protobuf
                             go-github-com-couchbase-moss
                             go-github-com-blevesearch-zapx-v16
                             go-github-com-blevesearch-zapx-v15
                             go-github-com-blevesearch-zapx-v14
                             go-github-com-blevesearch-zapx-v13
                             go-github-com-blevesearch-zapx-v12
                             go-github-com-blevesearch-zapx-v11
                             go-github-com-blevesearch-vellum
                             go-github-com-blevesearch-upsidedown-store-api
                             go-github-com-blevesearch-stempel
                             go-github-com-blevesearch-snowballstem
                             go-github-com-blevesearch-snowball
                             go-github-com-blevesearch-segment
                             go-github-com-blevesearch-scorch-segment-api-v2
                             go-github-com-blevesearch-gtreap
                             go-github-com-blevesearch-goleveldb
                             go-github-com-blevesearch-go-porterstemmer
                             go-github-com-blevesearch-go-metrics
                             go-github-com-blevesearch-go-faiss
                             go-github-com-blevesearch-geo
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-bits-and-blooms-bitset
                             go-github-com-roaringbitmap-roaring-v2))
    (home-page "https://github.com/blevesearch/bleve")
    (synopsis "bleve")
    (description "Package bleve is a library for indexing and searching text.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-bleve-index-api
  (package
    (name "go-github-com-blevesearch-bleve-index-api")
    (version "1.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blevesearch/bleve_index_api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fbnv6qyk32ds0rclrrxaczqmqqc3f06cjs1h7hn9n6a3c2q689g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/blevesearch/bleve_index_api"))
    (home-page "https://github.com/blevesearch/bleve_index_api")
    (synopsis "Bleve Index API")
    (description "Bleve supports a pluggable Index interface.")
    (license license:asl2.0)))

(define-public go-github-com-ethantkoenig-rupture
  (package
    (name "go-github-com-ethantkoenig-rupture")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ethantkoenig/rupture")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qc2rv4i1292f1fw3mfvf6zn9wy4nvbj6dla4lycdxdqvv066pd8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ethantkoenig/rupture"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-blevesearch-bleve-index-api
                             go-github-com-blevesearch-bleve-v2))
    (home-page "https://github.com/ethantkoenig/rupture")
    (synopsis "rupture")
    (description
     "An explosive companion to the
@@url{https://www.github.com/blevesearch/bleve,bleve indexing library}.")
    (license license:expat)))

(define-public go-git-sr-ht--mariusor-go-xsd-duration
  (package
    (name "go-git-sr-ht--mariusor-go-xsd-duration")
    (version "0.0.0-20220703122237-02e73435a078")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~mariusor/go-xsd-duration")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ibrjaf12bylma6phhdi46xcx7gxxrvmzggpspjrlq2xg7zzgv75"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "git.sr.ht/~mariusor/go-xsd-duration"))
    (home-page "https://git.sr.ht/~mariusor/go-xsd-duration")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-go-ap-errors
  (package
    (name "go-github-com-go-ap-errors")
    (version "0.0.0-20250527110557-c8db454e53fd")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ap/errors")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "195bcdpyjz46rcgq8f995k97ndik9cxsqa2azr4d33rglk4y126g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ap/errors"))
    (propagated-inputs (list go-github-com-valyala-fastjson
                             go-github-com-go-ap-jsonld))
    (home-page "https://github.com/go-ap/errors")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-valyala-fastjson
  (package
    (name "go-github-com-valyala-fastjson")
    (version "1.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/valyala/fastjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ly15rbdy9qmml39d8mazjvid3f13nhvj4v2zdlp13pn4gczdp3k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/valyala/fastjson"))
    (home-page "https://github.com/valyala/fastjson")
    (synopsis "fastjson - fast JSON parser and validator for Go")
    (description "Package fastjson provides fast JSON parsing.")
    (license license:expat)))

(define-public go-github-com-go-ap-activitypub
  (package
    (name "go-github-com-go-ap-activitypub")
    (version "0.0.0-20250527110644-1410ed93404d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ap/activitypub")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vr2vzf845kp5ipnp6f94ibk9pypwbjmwgcdwvbr8iwalc3l0mx0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ap/activitypub"))
    (propagated-inputs (list go-github-com-valyala-fastjson
                             go-github-com-go-ap-jsonld
                             go-github-com-go-ap-errors
                             go-git-sr-ht--mariusor-go-xsd-duration))
    (home-page "https://github.com/go-ap/activitypub")
    (synopsis "About GoActivityPub: Vocabulary")
    (description
     "This project is part of the @@url{https://github.com/go-ap,@code{GoActivityPub}}
library which helps with creating @code{ActivityPub} applications using the Go
programming language.")
    (license license:expat)))

(define-public go-github-com-go-ap-jsonld
  (package
    (name "go-github-com-go-ap-jsonld")
    (version "0.0.0-20221030091449-f2a191312c73")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ap/jsonld")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "033zghspl9fp52qc5impdcczzkxi64vjz9xibmvxjh1g0ky20s3m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ap/jsonld"))
    (home-page "https://github.com/go-ap/jsonld")
    (synopsis "JSON-ld for Go")
    (description
     "Package jsonld implements encoding and decoding of JSON as defined in
@@url{https://rfc-editor.org/rfc/rfc4627.html,RFC 4627}.  The mapping between
JSON and Go values is described in the documentation for the Marshal and
Unmarshal functions.")
    (license license:expat)))

(define-public go-github-com-go-chi-cors
  (package
    (name "go-github-com-go-chi-cors")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-chi/cors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13ya3h4lsd18hs58ark5q31dr2dpszgbjr371rph3yphfp2ddzlg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-chi/cors"))
    (home-page "https://github.com/go-chi/cors")
    (synopsis "CORS net/http middleware")
    (description
     "cors package is net/http handler to handle CORS related requests as defined by
@@url{http://www.w3.org/TR/cors/,http://www.w3.org/TR/cors/}.")
    (license license:expat)))

(define-public go-github-com-go-enry-go-oniguruma
  (package
    (name "go-github-com-go-enry-go-oniguruma")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-enry/go-oniguruma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wlgs5qms988f4q1h30c08f3w7jlnz76dlkp2shf02prgv4qv00f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-enry/go-oniguruma"))
    (home-page "https://github.com/go-enry/go-oniguruma")
    (synopsis "go-oniguruma")
    (description
     "This repository is a fork of
@@url{https://github.com/moovweb/rubex/tree/go1,moovweb/rubex} - a simple
regular expression library (based on
@@url{https://github.com/kkos/oniguruma,oniguruma}) that supports Ruby's regex
syntax.")
    (license license:expat)))

(define-public go-github-com-go-enry-go-enry
  (package
    (name "go-github-com-go-enry-go-enry")
    (version "2.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-enry/go-enry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zm82gywncx96nnhiymh1cxhplb1rgzgb7r3fmnky1fqg23nrsnv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-enry/go-enry/v2"
      #:unpack-path "github.com/go-enry/go-enry"))
    (propagated-inputs (list go-gopkg-in-yaml-v2
                             go-github-com-stretchr-testify
                             go-github-com-go-enry-go-oniguruma))
    (home-page "https://github.com/go-enry/go-enry")
    (synopsis "go-enry")
    (description "Package enry identifies programming languages.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-inflect
  (package
    (name "go-github-com-go-openapi-inflect")
    (version "0.21.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-openapi/inflect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmayn2qbl8dy7hk60xwwgkacpzv7ssm2s6xqn84kg4bnr6bbvhv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-openapi/inflect"))
    (home-page "https://github.com/go-openapi/inflect")
    (synopsis "inflect")
    (description "This package provides a package to pluralize words.")
    (license license:expat)))

(define-public go-github-com-go-swagger-scan-repo-boundary
  (package
    (name "go-github-com-go-swagger-scan-repo-boundary")
    (version "0.0.0-20180623220736-973b3573c013")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-swagger/scan-repo-boundary")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ipqv53s9piq5v5nsjmg8v7pzz4zinv2xkif7h0na84i9pnsccyn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-swagger/scan-repo-boundary"))
    (home-page "https://github.com/go-swagger/scan-repo-boundary")
    (synopsis "TestRepo")
    (description
     "This is a repo that is used in the tests of the go-swagger project.  It's is
only here to test finding files across repository boundaries.")
    (license license:asl2.0)))

(define-public go-github-com-toqueteos-webbrowser
  (package
    (name "go-github-com-toqueteos-webbrowser")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/toqueteos/webbrowser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j2hz0mq06v4vxksssg20yb34wwh24l55v2x7nplksfri1rmwbn0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/toqueteos/webbrowser"))
    (home-page "https://github.com/toqueteos/webbrowser")
    (synopsis "webbrowser")
    (description
     "Package webbrowser provides a simple API for opening web pages on your default
browser.")
    (license license:expat)))

(define-public go-github-com-go-swagger-go-swagger
  (package
    (name "go-github-com-go-swagger-go-swagger")
    (version "0.32.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-swagger/go-swagger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a07jp71w5aizvbnhm3l7l5rw2p9ybrpg6ah0fns34ijvhf1z89g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-swagger/go-swagger"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-gopkg-in-yaml-v2
                             go-golang-org-x-tools
                             go-golang-org-x-oauth2
                             go-golang-org-x-net
                             go-github-com-toqueteos-webbrowser
                             go-github-com-stretchr-testify
                             go-github-com-spf13-viper
                             go-github-com-spf13-pflag
                             go-github-com-spf13-cobra
                             go-github-com-kr-pretty
                             go-github-com-jessevdk-go-flags
                             go-github-com-gorilla-handlers
                             go-github-com-golang-jwt-jwt-v5
                             go-github-com-go-viper-mapstructure-v2
                             go-github-com-go-swagger-scan-repo-boundary
                             go-github-com-go-openapi-validate
                             go-github-com-go-openapi-swag
                             go-github-com-go-openapi-strfmt
                             go-github-com-go-openapi-spec
                             go-github-com-go-openapi-runtime
                             go-github-com-go-openapi-loads
                             go-github-com-go-openapi-inflect
                             go-github-com-go-openapi-errors
                             go-github-com-go-openapi-analysis
                             go-github-com-davecgh-go-spew
                             go-github-com-coreos-go-oidc-v3
                             go-github-com-masterminds-sprig-v3))
    (home-page "https://github.com/go-swagger/go-swagger")
    (synopsis "Swagger 2.0")
    (description
     "Package swagger (2.0) provides a powerful interface to your API.")
    (license license:asl2.0)))

(define-public go-github-com-pascaldekloe-name
  (package
    (name "go-github-com-pascaldekloe-name")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pascaldekloe/name")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x379sm8x16xls6dcn6l1kwb9aqnw1gqvnj7s4qcfg67hxgryw6d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pascaldekloe/name"))
    (home-page "https://github.com/pascaldekloe/name")
    (synopsis "About")
    (description
     "Package name implements various naming conventions.  The two categories are
delimiter-separated and letter case-separated words.  Each of the formatting
functions support both techniques for input, without any context.")
    (license license:cc0)))

(define-public go-github-com-dmarkham-enumer
  (package
    (name "go-github-com-dmarkham-enumer")
    (version "1.5.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmarkham/enumer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qjv1zjqbgsfcllcna1hcfgsisafhx4i5s59j6zp1nmb6q77hp6c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/dmarkham/enumer"))
    (propagated-inputs (list go-golang-org-x-tools
                             go-github-com-pascaldekloe-name))
    (home-page "https://github.com/dmarkham/enumer")
    (synopsis "Enumer")
    (description
     "Enumer is a tool to generate Go code that adds useful methods to Go enums
(constants with a specific type).  It started as a fork of Rob Pike’s Stringer
tool.")
    (license license:bsd-2)))

(define-public go-github-com-go-faster-city
  (package
    (name "go-github-com-go-faster-city")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-faster/city")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nssg8ynnsk4isrh88h4v8srzq46z8lqb867gr0dbpkasmq3d8dx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-faster/city"))
    (home-page "https://github.com/go-faster/city")
    (synopsis "city")
    (description "Package city implements @code{CityHash} in go.")
    (license license:expat)))

(define-public go-github-com-go-faster-errors
  (package
    (name "go-github-com-go-faster-errors")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-faster/errors")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s17dmrgrh20fkv1vj3p04pj48h8fs13ah648dpxy4zp6rkwwzwc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-faster/errors"))
    (home-page "https://github.com/go-faster/errors")
    (synopsis "errors")
    (description "Package errors implements functions to manipulate errors.")
    (license license:bsd-3)))

(define-public go-github-com-clickhouse-ch-go
  (package
    (name "go-github-com-clickhouse-ch-go")
    (version "0.66.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ClickHouse/ch-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c0lhhh4l78kyvj33h8xxsn3ssvqmf554p7rsbzaf5l4a5rd2q42"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ClickHouse/ch-go"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-go-uber-org-zap
                             go-go-uber-org-multierr
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-segmentio-asm
                             go-github-com-pierrec-lz4-v4
                             go-github-com-klauspost-compress
                             go-github-com-jackc-puddle-v2
                             go-github-com-hashicorp-go-version
                             go-github-com-google-uuid
                             go-github-com-go-faster-errors
                             go-github-com-go-faster-city
                             go-github-com-dustin-go-humanize
                             go-github-com-dmarkham-enumer
                             go-github-com-cenkalti-backoff-v4))
    (home-page "https://github.com/ClickHouse/ch-go")
    (synopsis "ch")
    (description "Package ch implements @code{ClickHouse} client.")
    (license license:asl2.0)))

(define-public go-github-com-bkaradzic-go-lz4
  (package
    (name "go-github-com-bkaradzic-go-lz4")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bkaradzic/go-lz4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vdid8v0c2v2qhrg9rzn3l7ya1h34jirrxfnir7gv7w6s4ivdvc1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bkaradzic/go-lz4"))
    (home-page "https://github.com/bkaradzic/go-lz4")
    (synopsis "go-lz4")
    (description
     "go-lz4 is port of LZ4 lossless compression algorithm to Go.  The original C code
is located at:.")
    (license license:bsd-2)))

(define-public go-github-com-cloudflare-golz4
  (package
    (name "go-github-com-cloudflare-golz4")
    (version "0.0.0-20240916140612-caecf3c00c06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudflare/golz4")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15nppvbav7kj3hh9qv9qbn15pd0c9lpljs5syl004cz6mif43as3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudflare/golz4"))
    (home-page "https://github.com/cloudflare/golz4")
    (synopsis "golz4")
    (description "Package lz4 implements compression using lz4.c and lz4hc.c.")
    (license license:bsd-3)))

(define-public go-github-com-clickhouse-clickhouse-go
  (package
    (name "go-github-com-clickhouse-clickhouse-go")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ClickHouse/clickhouse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15yf96mx3fkjyyasb8gjw6ml476k9qacp54bdjrb14pafz3p3rgf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ClickHouse/clickhouse-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-pierrec-lz4
                             go-github-com-jmoiron-sqlx
                             go-github-com-cloudflare-golz4
                             go-github-com-bkaradzic-go-lz4))
    (home-page "https://github.com/ClickHouse/clickhouse-go")
    (synopsis "ClickHouse")
    (description
     "Golang SQL database driver for @@url{https://clickhouse.yandex/,Yandex
@code{ClickHouse}}.")
    (license license:expat)))

(define-public go-github-com-mkevac-debugcharts
  (package
    (name "go-github-com-mkevac-debugcharts")
    (version "0.0.0-20191222103121-ae1c48aa8615")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mkevac/debugcharts")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10y8vmnppf9izp1mnin2axbp2lmdml4i4drzpni2r1az9sph3mpy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mkevac/debugcharts"))
    (propagated-inputs (list go-github-com-shirou-gopsutil
                             go-github-com-gorilla-websocket
                             go-github-com-gorilla-handlers))
    (home-page "https://github.com/mkevac/debugcharts")
    (synopsis "debugcharts")
    (description "Simple live charts for memory consumption and GC pauses.")
    (license license:expat)))

(define-public go-github-com-paulmach-protoscan
  (package
    (name "go-github-com-paulmach-protoscan")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paulmach/protoscan")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15sb6sc1v2haz44ccrlaxz9mgwc2hnyiy7gql46bisk25f8lx41b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/paulmach/protoscan"))
    (propagated-inputs (list go-google-golang-org-protobuf))
    (home-page "https://github.com/paulmach/protoscan")
    (synopsis "protoscan")
    (description
     "Package @@code{protoscan} is a low-level reader for
@@url{https://developers.google.com/protocol-buffers,protocol buffers} encoded
data in Golang.  The main feature is the support for lazy/conditional decoding
of fields.")
    (license license:expat)))

(define-public go-github-com-paulmach-orb
  (package
    (name "go-github-com-paulmach-orb")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paulmach/orb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nrc9d6ixn6v5s227fczpcqnpnn4ry5qj8vd6rq5dr3m77k4j7m2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/paulmach/orb"))
    (propagated-inputs (list go-go-mongodb-org-mongo-driver
                             go-github-com-paulmach-protoscan
                             go-github-com-gogo-protobuf))
    (home-page "https://github.com/paulmach/orb")
    (synopsis "orb")
    (description
     "Package @@code{orb} defines a set of types for working with 2d geo and
planar/projected geometric data in Golang.  There are a set of sub-packages that
use these types to do interesting things.  They each provide their own README
with extra info.")
    (license license:expat)))

(define-public go-github-com-cpuguy83-dockercfg
  (package
    (name "go-github-com-cpuguy83-dockercfg")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpuguy83/dockercfg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "055gxyq0wvyr9lap6rd49ijyg846mcpd1kwx9w69qj0pszvh2v96"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cpuguy83/dockercfg"))
    (home-page "https://github.com/cpuguy83/dockercfg")
    (synopsis "github.com/cpuguy83/dockercfg")
    (description
     "Go library to load docker CLI configs, auths, etc.  with minimal deps.  So far
the only deps are on the stdlib.")
    (license license:expat)))

(define-public go-github-com-docker-docker
  (package
    (name "go-github-com-docker-docker")
    (version "28.2.2+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/moby")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w859a1q82aw58a2kb4z124igxzg015dipi1vqyb4bj9s3c8yv33"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/docker"))
    (home-page "https://github.com/docker/docker")
    (synopsis "The Moby Project")
    (description
     "Moby is an open-source project created by Docker to enable and accelerate
software containerization.")
    (license license:asl2.0)))

(define-public go-github-com-moby-patternmatcher
  (package
    (name "go-github-com-moby-patternmatcher")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/patternmatcher")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s77wpsc6szr9qdpnpg9q65ibgjgj4b2d12hwf6wrwb39grcnbcz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/patternmatcher"))
    (home-page "https://github.com/moby/patternmatcher")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-azure-go-ansiterm
  (package
    (name "go-github-com-azure-go-ansiterm")
    (version "0.0.0-20250102033503-faa5f7b0171c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/go-ansiterm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11jwgg6zbwkij8vqcmia8pjk5s272f3zc89hh3g6v7k3vhk0lrp1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Azure/go-ansiterm"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/Azure/go-ansiterm")
    (synopsis "go-ansiterm")
    (description
     "This is a cross platform Ansi Terminal Emulation library.  It reads a stream of
Ansi characters and produces the appropriate function calls.  The results of the
function calls are platform dependent.")
    (license license:expat)))

(define-public go-github-com-moby-term
  (package
    (name "go-github-com-moby-term")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05g3dn1hbk9vxzp3dm752j8zn1gy61qzxm33nsj7xisa8s6v8vgw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/term"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-creack-pty
                             go-github-com-azure-go-ansiterm))
    (home-page "https://github.com/moby/term")
    (synopsis "term - utilities for dealing with terminals")
    (description
     "Package term provides structures and helper functions to work with terminal
(state, sizes).")
    (license license:asl2.0)))

(define-public go-github-com-ebitengine-purego
  (package
    (name "go-github-com-ebitengine-purego")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ebitengine/purego")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sy5cn56wxwv1qpl6vg5160hlk004m5wdlf18rfnj63573f5kivh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ebitengine/purego"))
    (home-page "https://github.com/ebitengine/purego")
    (synopsis "purego")
    (description
     "This package provides a library for calling C functions from Go without Cgo.")
    (license license:asl2.0)))

(define-public go-github-com-lufia-plan9stats
  (package
    (name "go-github-com-lufia-plan9stats")
    (version "0.0.0-20250317134145-8bc96cf8fc35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lufia/plan9stats")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17pgjlliylqb8hbxa9z06qldc5dggyb558vp188ha4ghf3j9xkhf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lufia/plan9stats"))
    (propagated-inputs (list go-github-com-google-go-cmp))
    (home-page "https://github.com/lufia/plan9stats")
    (synopsis "plan9stats")
    (description "Package stats provides statistic utilities for Plan 9.")
    (license license:bsd-3)))

(define-public go-github-com-power-devops-perfstat
  (package
    (name "go-github-com-power-devops-perfstat")
    (version "0.0.0-20240221224432-82ca36839d55")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/power-devops/perfstat")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lmsxb3wlf0088198mcljq6krqnvpy1qy8li833hhhkdbckywg5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/power-devops/perfstat"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/power-devops/perfstat")
    (synopsis #f)
    (description
     "Copyright 2020 Power-Devops.com.  All rights reserved.  Use of this source code
is governed by the license that can be found in the LICENSE file.")
    (license license:expat)))

(define-public go-github-com-go-ole-go-ole
  (package
    (name "go-github-com-go-ole-go-ole")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ole/go-ole")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vr62wwjp206sxah2l79l007s7n187fjzkrnwb85ivqmazfjspxl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ole/go-ole"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/go-ole/go-ole")
    (synopsis "Go OLE")
    (description
     "Go bindings for Windows COM using shared libraries instead of cgo.")
    (license license:expat)))

(define-public go-github-com-yusufpapurcu-wmi
  (package
    (name "go-github-com-yusufpapurcu-wmi")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yusufpapurcu/wmi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c6xjjad3zxddw8x910aiy5h9h8ndlal99cxn47ddrwn6c307rip"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yusufpapurcu/wmi"))
    (propagated-inputs (list go-github-com-go-ole-go-ole))
    (home-page "https://github.com/yusufpapurcu/wmi")
    (synopsis "wmi")
    (description "Package wmi provides a WQL interface for WMI on Windows.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil
  (package
    (name "go-github-com-shirou-gopsutil")
    (version "4.25.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10k11gzd1bn06925vin7g2781b64s1jx1wmn23yjd67f9iv1pksq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/shirou/gopsutil/v4"
      #:unpack-path "github.com/shirou/gopsutil"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-yusufpapurcu-wmi
                             go-github-com-tklauser-go-sysconf
                             go-github-com-stretchr-testify
                             go-github-com-power-devops-perfstat
                             go-github-com-lufia-plan9stats
                             go-github-com-google-go-cmp
                             go-github-com-ebitengine-purego))
    (home-page "https://github.com/shirou/gopsutil")
    (synopsis "gopsutil: psutil for golang")
    (description "SPDX-License-Identifier: BSD-3-Clause.")
    (license license:bsd-3)))

(define-public go-github-com-testcontainers-testcontainers-go
  (package
    (name "go-github-com-testcontainers-testcontainers-go")
    (version "0.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/testcontainers/testcontainers-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z73h94gzl17da9fsi7y12h0b2i6hps4g8kwvx0xdkpfhmigs49r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/testcontainers/testcontainers-go"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-shirou-gopsutil-v4
                             go-github-com-opencontainers-image-spec
                             go-github-com-moby-term
                             go-github-com-moby-patternmatcher
                             go-github-com-magiconair-properties
                             go-github-com-google-uuid
                             go-github-com-docker-go-connections
                             go-github-com-docker-docker
                             go-github-com-cpuguy83-dockercfg
                             go-github-com-containerd-platforms
                             go-github-com-cenkalti-backoff-v4
                             go-dario-cat-mergo))
    (home-page "https://github.com/testcontainers/testcontainers-go")
    (synopsis "Testcontainers")
    (description
     "is a Go package that makes it simple to create and clean up container-based
dependencies for automated integration/smoke tests.  The clean, easy-to-use API
enables developers to programmatically define containers that should be run as
part of a test and clean up those resources when the test is done.")
    (license license:expat)))

(define-public go-github-com-clickhouse-clickhouse-go
  (package
    (name "go-github-com-clickhouse-clickhouse-go")
    (version "2.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ClickHouse/clickhouse-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x38p1kl0iz4qva0iv03r2akslygbi33xvk3fg96zxs6n0sdlh8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ClickHouse/clickhouse-go/v2"
      #:unpack-path "github.com/ClickHouse/clickhouse-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-golang-org-x-net
                             go-go-opentelemetry-io-otel-trace
                             go-github-com-testcontainers-testcontainers-go
                             go-github-com-stretchr-testify
                             go-github-com-shopspring-decimal
                             go-github-com-paulmach-orb
                             go-github-com-mkevac-debugcharts
                             go-github-com-google-uuid
                             go-github-com-docker-go-units
                             go-github-com-docker-go-connections
                             go-github-com-docker-docker
                             go-github-com-andybalholm-brotli
                             go-github-com-clickhouse-clickhouse-go
                             go-github-com-clickhouse-ch-go))
    (home-page "https://github.com/ClickHouse/clickhouse-go")
    (synopsis "ClickHouse")
    (description
     "Golang SQL database client for @@url{https://clickhouse.com/,@code{ClickHouse}}.")
    (license license:asl2.0)))

(define-public go-github-com-denisenkom-go-mssqldb
  (package
    (name "go-github-com-denisenkom-go-mssqldb")
    (version "0.12.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/denisenkom/go-mssqldb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12zd98r3p9nvjd1cnlzfzdifk4smkq8zffw7qidw1ni90zl26gy2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/denisenkom/go-mssqldb"))
    (propagated-inputs (list go-golang-org-x-crypto
                        go-github-com-golang-sql-sqlexp
                        go-github-com-golang-sql-civil
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/denisenkom/go-mssqldb")
    (synopsis "A pure Go MSSQL driver for Go's database/sql package")
    (description
     "package mssql implements the TDS protocol used to connect to MS SQL Server
(sqlserver) database servers.")
    (license license:bsd-3)))

(define-public go-cloud-google-com-go-auth-oauth2adapt
  (package
    (name "go-cloud-google-com-go-auth-oauth2adapt")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth/oauth2adapt"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "109szg097fn42qpsmrmd29iwsdh2yrjh9krq8mjm02fnm7l18lc4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/auth/oauth2adapt"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-auth))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package oauth2adapt helps converts types used in
@@url{/cloud.google.com/go/auth,cloud.google.com/go/auth} and
@@url{/golang.org/x/oauth2,golang.org/x/oauth2}.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accessapproval
  (package
    (name "go-cloud-google-com-go-accessapproval")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accessapproval"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/accessapproval"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Approval API")
    (description "Go Client Library for Access Approval API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-aiplatform
  (package
    (name "go-cloud-google-com-go-aiplatform")
    (version "1.90.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "aiplatform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/aiplatform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Vertex AI API")
    (description "Go Client Library for Vertex AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-analytics
  (package
    (name "go-cloud-google-com-go-analytics")
    (version "0.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "analytics"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/analytics"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Analytics API")
    (description "Go Client Library for Analytics API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigateway
  (package
    (name "go-cloud-google-com-go-apigateway")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigateway"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigateway"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "API Gateway API")
    (description "Go Client Library for API Gateway API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeconnect
  (package
    (name "go-cloud-google-com-go-apigeeconnect")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigeeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Connect API")
    (description "Go Client Library for Apigee Connect API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeregistry
  (package
    (name "go-cloud-google-com-go-apigeeregistry")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigeeregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Registry API")
    (description "Go Client Library for Apigee Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-appengine
  (package
    (name "go-cloud-google-com-go-appengine")
    (version "1.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "appengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/appengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "App Engine Admin API")
    (description "Go Client Library for App Engine Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-area120
  (package
    (name "go-cloud-google-com-go-area120")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "area120"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/area120"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Area120 API")
    (description "Go Client Library for Area120 API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-artifactregistry
  (package
    (name "go-cloud-google-com-go-artifactregistry")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "artifactregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/artifactregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Artifact Registry API")
    (description "Go Client Library for Artifact Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accesscontextmanager
  (package
    (name "go-cloud-google-com-go-accesscontextmanager")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accesscontextmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/accesscontextmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Context Manager API")
    (description "Go Client Library for Access Context Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-asset
  (package
    (name "go-cloud-google-com-go-asset")
    (version "1.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "asset"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/asset"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-accesscontextmanager))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Asset API")
    (description "Go Client Library for Cloud Asset API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-assuredworkloads
  (package
    (name "go-cloud-google-com-go-assuredworkloads")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "assuredworkloads"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/assuredworkloads"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Assured Workloads API")
    (description "Go Client Library for Assured Workloads API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-automl
  (package
    (name "go-cloud-google-com-go-automl")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "automl"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/automl"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud AutoML API")
    (description "Go Client Library for Cloud @code{AutoML} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-baremetalsolution
  (package
    (name "go-cloud-google-com-go-baremetalsolution")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "baremetalsolution"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/baremetalsolution"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Bare Metal Solution API")
    (description "Go Client Library for Bare Metal Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-batch
  (package
    (name "go-cloud-google-com-go-batch")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "batch"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/batch"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Batch API")
    (description "Go Client Library for Batch API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-beyondcorp
  (package
    (name "go-cloud-google-com-go-beyondcorp")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "beyondcorp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/beyondcorp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "BeyondCorp API")
    (description "Go Client Library for @code{BeyondCorp} API.")
    (license license:asl2.0)))

(define-public go-github-com-apache-thrift
  (package
    (name "go-github-com-apache-thrift")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vkmm7g87lmgsbflgnmrvjrj8lvk87s3mfn93hl13zh07pw0wq40"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/apache/thrift"))
    (home-page "https://github.com/apache/thrift")
    (synopsis "Apache Thrift")
    (description
     "Thrift is a lightweight, language-independent software stack for point-to-point
RPC implementation.  Thrift provides clean abstractions and implementations for
data transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates code
across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license license:asl2.0)))

(define-public go-github-com-minio-asm2plan9s
  (package
    (name "go-github-com-minio-asm2plan9s")
    (version "0.0.0-20200509001527-cdd76441f9d8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/asm2plan9s")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i635ipzfqy7cyj68sl3mmqbnjqgyrhjxpyp62z2dbm34i42pfbg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/asm2plan9s"))
    (home-page "https://github.com/minio/asm2plan9s")
    (synopsis "asm2plan9s")
    (description
     "Tool to generate BYTE sequences for Go assembly as generated by YASM/GAS (for
Intel) or GAS (for ARM).")
    (license license:asl2.0)))

(define-public go-github-com-minio-c2goasm
  (package
    (name "go-github-com-minio-c2goasm")
    (version "0.0.0-20190812172519-36a3d3bbc4f3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/c2goasm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aw8g7455r7av7s4sdc57yyd2d5298linppx8m4cfhrgmd6rblzf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/c2goasm"))
    (home-page "https://github.com/minio/c2goasm")
    (synopsis "c2goasm: C to Go Assembly")
    (description
     "This is a tool to convert assembly as generated by a C/C++ compiler into Golang
assembly.  It is meant to be used in combination with
@@url{https://github.com/minio/asm2plan9s,asm2plan9s} in order to automatically
generate pure Go wrappers for C/C++ code (that may for instance take advantage
of compiler SIMD intrinsics or @@code{template<>} code).")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-xxh3
  (package
    (name "go-github-com-zeebo-xxh3")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/xxh3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gy666r5v1d1n2cfig9plhyp7z09f06k6mr5lrf0mk6psk6bnwgi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/xxh3"))
    (propagated-inputs (list go-github-com-zeebo-assert
                             go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/zeebo/xxh3")
    (synopsis "XXH3")
    (description
     "This package is a port of the
@@url{https://github.com/Cyan4973/@code{xxHash,xxh3}} library to Go.")
    (license license:bsd-2)))

(define-public go-github-com-hamba-avro
  (package
    (name "go-github-com-hamba-avro")
    (version "2.29.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hamba/avro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hiv4aydi8x1s4vjasfrrnbj328l2g6g2x5smag1crl1zz57v115"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/hamba/avro/v2"
      #:unpack-path "github.com/hamba/avro"))
    (propagated-inputs (list go-golang-org-x-tools
                             go-github-com-stretchr-testify
                             go-github-com-modern-go-reflect2
                             go-github-com-klauspost-compress
                             go-github-com-json-iterator-go
                             go-github-com-golang-snappy
                             go-github-com-go-viper-mapstructure-v2
                             go-github-com-ettle-strcase))
    (home-page "https://github.com/hamba/avro")
    (synopsis "Overview")
    (description
     "Package avro implements encoding and decoding of Avro as defined by the Avro
specification.")
    (license license:expat)))

(define-public go-github-com-substrait-io-substrait
  (package
    (name "go-github-com-substrait-io-substrait")
    (version "0.73.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c25493yvzjij6wszd9xhb2cxzl166ywm9a5i9g2ha8db8bky6gy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/substrait-io/substrait"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/substrait-io/substrait")
    (synopsis "Substrait")
    (description
     "Package substrait provides access to Substrait artifacts via embed.FS. Use
@code{substrait.GetSubstraitFS()} to retrieve the embed.FS object.")
    (license license:asl2.0)))

(define-public go-github-com-substrait-io-substrait-go
  (package
    (name "go-github-com-substrait-io-substrait-go")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "032d2d5dgapv5dypp3h6m0zzm20x79ypljwc02fw5gcpbli3x2m7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/substrait-io/substrait-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-protobuf
                             go-golang-org-x-exp
                             go-github-com-substrait-io-substrait
                             go-github-com-stretchr-testify
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-goccy-go-yaml
                             go-github-com-creasty-defaults
                             go-github-com-cockroachdb-apd-v3
                             go-github-com-alecthomas-participle-v2))
    (home-page "https://github.com/substrait-io/substrait-go")
    (synopsis "substrait-go")
    (description
     "Package substraitgo contains the experimental go bindings for substrait
(@@url{https://substrait.io,https://substrait.io}).")
    (license license:asl2.0)))

(define-public go-github-com-apache-arrow-go
  (package
    (name "go-github-com-apache-arrow-go")
    (version "15.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (go-version->git-ref version
                                          #:subdir "go"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckv1lk4mm9smp8z545yqz32vs8psqsd9crg038xfz9ifwzq9vcz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/apache/arrow/go/v15"
      #:unpack-path "github.com/apache/arrow"))
    (propagated-inputs (list go-github-com-tidwall-sjson
                             go-github-com-substrait-io-substrait-go
                             go-github-com-hamba-avro-v2
                             go-github-com-google-uuid
                             go-modernc-org-sqlite
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-gonum-org-v1-gonum
                             go-golang-org-x-xerrors
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-golang-org-x-exp
                             go-github-com-zeebo-xxh3
                             go-github-com-stretchr-testify
                             go-github-com-pierrec-lz4-v4
                             go-github-com-minio-c2goasm
                             go-github-com-minio-asm2plan9s
                             go-github-com-klauspost-cpuid-v2
                             go-github-com-klauspost-compress
                             go-github-com-klauspost-asmfmt
                             go-github-com-google-flatbuffers
                             go-github-com-golang-snappy
                             go-github-com-goccy-go-json
                             go-github-com-docopt-docopt-go
                             go-github-com-apache-thrift
                             go-github-com-andybalholm-brotli
                             go-github-com-johncgriffin-overflow))
    (home-page "https://github.com/apache/arrow")
    (synopsis "Apache Arrow for Go")
    (description
     "@@url{https://arrow.apache.org,Apache Arrow} is a cross-language development
platform for in-memory data.  It specifies a standardized language-independent
columnar memory format for flat and hierarchical data, organized for efficient
analytic operations on modern hardware.  It also provides computational
libraries and zero-copy streaming messaging and inter-process communication.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-bigquery
  (package
    (name "go-cloud-google-com-go-bigquery")
    (version "1.69.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigquery"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "021dnqqs86va6gqnaa1p4wmsvphkzs36malbw23r913a26r77iqf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/bigquery"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-xerrors
                             go-golang-org-x-sync
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-apache-arrow-go-v15
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "BigQuery")
    (description
     "Package bigquery provides a client for the @code{BigQuery} service.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-cloud-bigtable-clients-test
  (package
    (name "go-github-com-googleapis-cloud-bigtable-clients-test")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/cloud-bigtable-clients-test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05nvmwmhj94na0a7nzmjh7703qji5pf9a0sjf219hwcl794hi26q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/cloud-bigtable-clients-test"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-bigtable))
    (home-page "https://github.com/googleapis/cloud-bigtable-clients-test")
    (synopsis "Test Framework for Cloud Bigtable Client Libraries")
    (description
     "This repository contains the test framework to validate the correctness of Cloud
Bigtable @@url{https://cloud.google.com/bigtable/docs/reference/libraries,client
libraries}.  Specifically, all of the client libraries should exhibit correct
and consistent behaviors when interacting with the server (e.g. retry on
transient error) However, writing test cases in every language would present
maintainability and scalability challenges.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-bigtable
  (package
    (name "go-cloud-google-com-go-bigtable")
    (version "1.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigtable"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0al2fpb32izmm2ydhwx752rxnag594xx5i88ajxi9r31sy05vckx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/bigtable"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-rsc-io-binaryregexp
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-cloud-bigtable-clients-test
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-google-btree
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package bigtable is an API to Google Cloud Bigtable.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-billing
  (package
    (name "go-cloud-google-com-go-billing")
    (version "1.20.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "billing"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/billing"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Billing API")
    (description "Go Client Library for Cloud Billing API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-binaryauthorization
  (package
    (name "go-cloud-google-com-go-binaryauthorization")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "binaryauthorization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/binaryauthorization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Binary Authorization API")
    (description "Go Client Library for Binary Authorization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-certificatemanager
  (package
    (name "go-cloud-google-com-go-certificatemanager")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "certificatemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/certificatemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Certificate Manager API")
    (description "Go Client Library for Certificate Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-channel
  (package
    (name "go-cloud-google-com-go-channel")
    (version "1.19.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "channel"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/channel"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Channel API")
    (description "Go Client Library for Cloud Channel API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudbuild
  (package
    (name "go-cloud-google-com-go-cloudbuild")
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudbuild"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/cloudbuild"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Build API")
    (description "Go Client Library for Cloud Build API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-clouddms
  (package
    (name "go-cloud-google-com-go-clouddms")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "clouddms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/clouddms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Database Migration API")
    (description "Go Client Library for Database Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudtasks
  (package
    (name "go-cloud-google-com-go-cloudtasks")
    (version "1.13.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudtasks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/cloudtasks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Tasks API")
    (description "Go Client Library for Cloud Tasks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-compute
  (package
    (name "go-cloud-google-com-go-compute")
    (version "1.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "compute"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/compute"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Compute API")
    (description "Go Client Library for Compute API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-contactcenterinsights
  (package
    (name "go-cloud-google-com-go-contactcenterinsights")
    (version "1.17.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "contactcenterinsights"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/contactcenterinsights"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Contact Center AI Insights API")
    (description "Go Client Library for Contact Center AI Insights API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-container
  (package
    (name "go-cloud-google-com-go-container")
    (version "1.43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "container"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/container"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Kubernetes Engine API")
    (description
     "Package container contains a deprecated Google Container Engine client.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-grafeas
  (package
    (name "go-cloud-google-com-go-grafeas")
    (version "0.3.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "grafeas"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/grafeas"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Grafeas API")
    (description "Go Client Library for Grafeas API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-containeranalysis
  (package
    (name "go-cloud-google-com-go-containeranalysis")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "containeranalysis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/containeranalysis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-grafeas
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Container Analysis API")
    (description "Go Client Library for Container Analysis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datacatalog
  (package
    (name "go-cloud-google-com-go-datacatalog")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datacatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10xfkc3igsw95fs13k8w0qjig6fsfpd1j9kar43jx74wqlhs02rb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datacatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Data Catalog API")
    (description "Go Client Library for Google Cloud Data Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataflow
  (package
    (name "go-cloud-google-com-go-dataflow")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataflow API")
    (description "Go Client Library for Dataflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataform
  (package
    (name "go-cloud-google-com-go-dataform")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataform API")
    (description "Go Client Library for Dataform API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datafusion
  (package
    (name "go-cloud-google-com-go-datafusion")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datafusion"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datafusion"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Fusion API")
    (description "Go Client Library for Cloud Data Fusion API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datalabeling
  (package
    (name "go-cloud-google-com-go-datalabeling")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datalabeling"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datalabeling"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data Labeling API")
    (description "Go Client Library for Data Labeling API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataplex
  (package
    (name "go-cloud-google-com-go-dataplex")
    (version "1.25.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataplex"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataplex"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataplex API")
    (description "Go Client Library for Cloud Dataplex API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataproc
  (package
    (name "go-cloud-google-com-go-dataproc")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataproc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataproc/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataproc API")
    (description "Go Client Library for Cloud Dataproc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataqna
  (package
    (name "go-cloud-google-com-go-dataqna")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataqna"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataqna"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data QnA API")
    (description "Go Client Library for Data @code{QnA} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastore
  (package
    (name "go-cloud-google-com-go-datastore")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fj48fav12jrg3dzbbal8h5rv3xhgq0kc9vnihnxdj5nvbig9y8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/datastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Datastore")
    (description
     "Package datastore provides a client for Google Cloud Datastore.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastream
  (package
    (name "go-cloud-google-com-go-datastream")
    (version "1.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datastream"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Datastream API")
    (description "Go Client Library for Datastream API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-deploy
  (package
    (name "go-cloud-google-com-go-deploy")
    (version "1.27.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "deploy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/deploy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Deploy API")
    (description "Go Client Library for Google Cloud Deploy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dialogflow
  (package
    (name "go-cloud-google-com-go-dialogflow")
    (version "1.68.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dialogflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dialogflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dialogflow API")
    (description "Go Client Library for Dialogflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dlp
  (package
    (name "go-cloud-google-com-go-dlp")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dlp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dlp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Loss Prevention (DLP) API")
    (description "Go Client Library for Cloud Data Loss Prevention (DLP) API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-documentai
  (package
    (name "go-cloud-google-com-go-documentai")
    (version "1.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "documentai"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10xfkc3igsw95fs13k8w0qjig6fsfpd1j9kar43jx74wqlhs02rb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/documentai"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Document AI API")
    (description "Go Client Library for Cloud Document AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-domains
  (package
    (name "go-cloud-google-com-go-domains")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "domains"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/domains"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Domains API")
    (description "Go Client Library for Cloud Domains API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-edgecontainer
  (package
    (name "go-cloud-google-com-go-edgecontainer")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "edgecontainer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/edgecontainer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Distributed Cloud Edge Container API")
    (description "Go Client Library for Distributed Cloud Edge Container API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-errorreporting
  (package
    (name "go-cloud-google-com-go-errorreporting")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "errorreporting"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00bdhr81cr37vy0llh0sifhx0ya5izhdwy95y72ykhavvivlksyd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/errorreporting"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Error Reporting API")
    (description
     "Package errorreporting is a Google Cloud Error Reporting library.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-essentialcontacts
  (package
    (name "go-cloud-google-com-go-essentialcontacts")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "essentialcontacts"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/essentialcontacts"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Essential Contacts API")
    (description "Go Client Library for Essential Contacts API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-eventarc
  (package
    (name "go-cloud-google-com-go-eventarc")
    (version "1.15.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "eventarc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/eventarc"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Eventarc API")
    (description "Go Client Library for Eventarc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-filestore
  (package
    (name "go-cloud-google-com-go-filestore")
    (version "1.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "filestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/filestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Filestore API")
    (description "Go Client Library for Cloud Filestore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-firestore
  (package
    (name "go-cloud-google-com-go-firestore")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "firestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cm1yn7d26cqar9nf1yw9ml3kqdqbhy2rv9m441sqgwbm44b10da"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/firestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description
     "Package firestore provides a client for reading and writing to a Cloud Firestore
database.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-functions
  (package
    (name "go-cloud-google-com-go-functions")
    (version "1.19.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "functions"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/functions"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Functions API")
    (description "Go Client Library for Cloud Functions API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkebackup
  (package
    (name "go-cloud-google-com-go-gkebackup")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkebackup"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkebackup"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Backup for GKE API")
    (description "Go Client Library for Backup for GKE API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkeconnect
  (package
    (name "go-cloud-google-com-go-gkeconnect")
    (version "0.12.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Connect APIs")
    (description "Go Client Library for GKE Connect APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkehub
  (package
    (name "go-cloud-google-com-go-gkehub")
    (version "0.15.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkehub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkehub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Hub")
    (description "Go Client Library for GKE Hub.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkemulticloud
  (package
    (name "go-cloud-google-com-go-gkemulticloud")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkemulticloud"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkemulticloud"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Anthos Multi-Cloud API")
    (description "Go Client Library for Anthos Multi-Cloud API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gsuiteaddons
  (package
    (name "go-cloud-google-com-go-gsuiteaddons")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gsuiteaddons"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gsuiteaddons"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Workspace Add-ons API")
    (description "Go Client Library for Google Workspace Add-ons API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iap
  (package
    (name "go-cloud-google-com-go-iap")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iap"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iap"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Identity-Aware Proxy API")
    (description "Go Client Library for Cloud Identity-Aware Proxy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-ids
  (package
    (name "go-cloud-google-com-go-ids")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "ids"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/ids"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IDS API")
    (description "Go Client Library for Cloud IDS API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iot
  (package
    (name "go-cloud-google-com-go-iot")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iot"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iot"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IoT API")
    (description "Go Client Library for Cloud @code{IoT} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-language
  (package
    (name "go-cloud-google-com-go-language")
    (version "1.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "language"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/language"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Natural Language API")
    (description "Go Client Library for Cloud Natural Language API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-lifesciences
  (package
    (name "go-cloud-google-com-go-lifesciences")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "lifesciences"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/lifesciences"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Life Sciences API")
    (description "Go Client Library for Cloud Life Sciences API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-logging
  (package
    (name "go-cloud-google-com-go-logging")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "logging"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18v0lcwn97i8amqd054i55xnx9gclcj9gwyrrqdwjrh5g8kmc19c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/logging"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-compute-metadata
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Logging")
    (description
     "Package logging contains a Cloud Logging client suitable for writing logs.  For
reading logs, and working with sinks, metrics and monitored resources, see
package cloud.google.com/go/logging/logadmin.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-managedidentities
  (package
    (name "go-cloud-google-com-go-managedidentities")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "managedidentities"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/managedidentities"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Managed Service for Microsoft Active Directory API")
    (description
     "Go Client Library for Managed Service for Microsoft Active Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-maps
  (package
    (name "go-cloud-google-com-go-maps")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "maps"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/maps"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Maps Platform APIs")
    (description "Go Client Library for Google Maps Platform APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-mediatranslation
  (package
    (name "go-cloud-google-com-go-mediatranslation")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "mediatranslation"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/mediatranslation"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Media Translation API")
    (description "Go Client Library for Media Translation API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-memcache
  (package
    (name "go-cloud-google-com-go-memcache")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "memcache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/memcache"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Memorystore for Memcached API")
    (description "Go Client Library for Cloud Memorystore for Memcached API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-metastore
  (package
    (name "go-cloud-google-com-go-metastore")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "metastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/metastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataproc Metastore API")
    (description "Go Client Library for Dataproc Metastore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkconnectivity
  (package
    (name "go-cloud-google-com-go-networkconnectivity")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkconnectivity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networkconnectivity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Connectivity API")
    (description "Go Client Library for Network Connectivity API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkmanagement
  (package
    (name "go-cloud-google-com-go-networkmanagement")
    (version "1.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkmanagement"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networkmanagement"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Management API")
    (description "Go Client Library for Network Management API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networksecurity
  (package
    (name "go-cloud-google-com-go-networksecurity")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networksecurity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networksecurity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Security API")
    (description "Go Client Library for Network Security API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-notebooks
  (package
    (name "go-cloud-google-com-go-notebooks")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "notebooks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/notebooks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Notebooks API")
    (description "Go Client Library for Notebooks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-optimization
  (package
    (name "go-cloud-google-com-go-optimization")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "optimization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/optimization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Optimization API")
    (description "Go Client Library for Cloud Optimization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orchestration
  (package
    (name "go-cloud-google-com-go-orchestration")
    (version "1.11.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orchestration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zvp47kv8wyhwhkgqy89h5kzf94rdk3dbsqm0hggl0b5mq6pc1iw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/orchestration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Composer API")
    (description "Go Client Library for Cloud Composer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orgpolicy
  (package
    (name "go-cloud-google-com-go-orgpolicy")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orgpolicy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n1bmmsbyfljw4x4p0gfcgpqw17wca6r4dkz98xn41mbhpi6dhkk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/orgpolicy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Organization Policy API")
    (description "Go Client Library for Organization Policy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-osconfig
  (package
    (name "go-cloud-google-com-go-osconfig")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "osconfig"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/osconfig"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "OS Config API")
    (description "Go Client Library for OS Config API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-oslogin
  (package
    (name "go-cloud-google-com-go-oslogin")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "oslogin"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/oslogin"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud OS Login API")
    (description "Go Client Library for Cloud OS Login API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-phishingprotection
  (package
    (name "go-cloud-google-com-go-phishingprotection")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "phishingprotection"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/phishingprotection"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Phishing Protection API")
    (description "Go Client Library for Phishing Protection API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-policytroubleshooter
  (package
    (name "go-cloud-google-com-go-policytroubleshooter")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "policytroubleshooter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/policytroubleshooter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Policy Troubleshooter API")
    (description "Go Client Library for Policy Troubleshooter API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-privatecatalog
  (package
    (name "go-cloud-google-com-go-privatecatalog")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "privatecatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/privatecatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Private Catalog API")
    (description "Go Client Library for Cloud Private Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-kms
  (package
    (name "go-cloud-google-com-go-kms")
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "kms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/kms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Key Management Service (KMS) API")
    (description
     "Go Client Library for Cloud Key Management Service (KMS) API.")
    (license license:asl2.0)))

(define-public go-go-einride-tech-aip
  (package
    (name "go-go-einride-tech-aip")
    (version "0.70.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/einride/aip-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09g8hhlclqccwixn39fjd1n5ip719dwccydxdjsi8xfjy8lbyrr7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.einride.tech/aip"))
    (propagated-inputs (list go-gotest-tools-v3
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-github-com-stoewer-go-strcase
                             go-github-com-google-uuid))
    (home-page "https://go.einride.tech/aip")
    (synopsis "AIP Go")
    (description
     "Package aip provides primitives for implementing API Improvement Proposals
(AIP).")
    (license license:expat)))

(define-public go-cloud-google-com-go-pubsub
  (package
    (name "go-cloud-google-com-go-pubsub")
    (version "1.49.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01bllmiirmmkw040i1pxfc71zsv8lm3cbziqv1vk5b4ryys6smky"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/pubsub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-go-einride-tech-aip
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Pub/Sub")
    (description
     "Package pubsub provides an easy way to publish and receive Google Cloud Pub/Sub
messages, hiding the details of the underlying server RPCs.  Pub/Sub is a
many-to-many, asynchronous messaging system that decouples senders and
receivers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-pubsublite
  (package
    (name "go-cloud-google-com-go-pubsublite")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsublite"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7ycyzxbk6k4s63r0f8crb8i4jcc3lsk5n2wcnfdk6qkzs15572"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/pubsublite"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Pub/Sub Lite")
    (description
     "Package pubsublite provides an easy way to publish and receive messages using
the Pub/Sub Lite service.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recaptchaenterprise
  (package
    (name "go-cloud-google-com-go-recaptchaenterprise")
    (version "2.20.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recaptchaenterprise"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zvp47kv8wyhwhkgqy89h5kzf94rdk3dbsqm0hggl0b5mq6pc1iw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recaptchaenterprise/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "reCAPTCHA Enterprise API")
    (description "Go Client Library for @code{reCAPTCHA} Enterprise API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommendationengine
  (package
    (name "go-cloud-google-com-go-recommendationengine")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommendationengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recommendationengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommendations AI")
    (description "Go Client Library for Recommendations AI.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommender
  (package
    (name "go-cloud-google-com-go-recommender")
    (version "1.13.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommender"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recommender"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommender API")
    (description "Go Client Library for Recommender API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-redis
  (package
    (name "go-cloud-google-com-go-redis")
    (version "1.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "redis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/redis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Memorystore for Redis API")
    (description
     "Go Client Library for Google Cloud Memorystore for Redis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcemanager
  (package
    (name "go-cloud-google-com-go-resourcemanager")
    (version "1.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/resourcemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Resource Manager API")
    (description "Go Client Library for Cloud Resource Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcesettings
  (package
    (name "go-cloud-google-com-go-resourcesettings")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcesettings"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w04dgxk0lx5k4s255ladf5w8n2nivvhp5vlyb2va96j5aa8j2q5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/resourcesettings"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Resource Settings API")
    (description "Go Client Library for Resource Settings API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-retail
  (package
    (name "go-cloud-google-com-go-retail")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "retail"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/retail"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Retail API")
    (description "Go Client Library for Retail API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-run
  (package
    (name "go-cloud-google-com-go-run")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "run"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/run"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Run Admin API")
    (description "Go Client Library for Cloud Run Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-scheduler
  (package
    (name "go-cloud-google-com-go-scheduler")
    (version "1.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "scheduler"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/scheduler"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Scheduler API")
    (description "Go Client Library for Cloud Scheduler API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-secretmanager
  (package
    (name "go-cloud-google-com-go-secretmanager")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "secretmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/secretmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Secret Manager API")
    (description "Go Client Library for Secret Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-security
  (package
    (name "go-cloud-google-com-go-security")
    (version "1.18.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "security"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/security"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security APIs")
    (description "Go Client Library for Security APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-securitycenter
  (package
    (name "go-cloud-google-com-go-securitycenter")
    (version "1.36.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "securitycenter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/securitycenter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security Command Center API")
    (description "Go Client Library for Security Command Center API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-servicedirectory
  (package
    (name "go-cloud-google-com-go-servicedirectory")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "servicedirectory"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/servicedirectory"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Service Directory API")
    (description "Go Client Library for Service Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-shell
  (package
    (name "go-cloud-google-com-go-shell")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "shell"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/shell"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Shell API")
    (description "Go Client Library for Cloud Shell API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
  (package
    (name "go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
             (commit (go-version->git-ref version
                                          #:subdir "grpcgcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rhcs9fj78z6fsfbyzra1l0n4bwmn74rg4shqhx91jhg2hkhazd7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/GoogleCloudPlatform/grpc-gcp-go/grpcgcp"
      #:unpack-path "github.com/GoogleCloudPlatform/grpc-gcp-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-google-go-cmp
                             go-github-com-golang-mock))
    (home-page "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
    (synopsis "How to test Spanner integration")
    (description
     "Package grpcgcp provides grpc supports for Google Cloud APIs.  For now it
provides connection management with affinity support.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-spanner
  (package
    (name "go-cloud-google-com-go-spanner")
    (version "1.82.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "spanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lii7881p7jhvfizdf8sf39yl15nw4ganbmdd6rid4ld2vm87pjj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/spanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-go-opencensus-io
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Spanner")
    (description
     "Package spanner provides a client for reading and writing to Cloud Spanner
databases.  See the packages under admin for clients that operate on databases
and instances.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-speech
  (package
    (name "go-cloud-google-com-go-speech")
    (version "1.27.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "speech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/speech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Speech-to-Text API")
    (description "Go Client Library for Cloud Speech-to-Text API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-storagetransfer
  (package
    (name "go-cloud-google-com-go-storagetransfer")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storagetransfer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/storagetransfer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Storage Transfer API")
    (description "Go Client Library for Storage Transfer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-talent
  (package
    (name "go-cloud-google-com-go-talent")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "talent"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/talent"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Talent Solution API")
    (description "Go Client Library for Cloud Talent Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-texttospeech
  (package
    (name "go-cloud-google-com-go-texttospeech")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "texttospeech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/texttospeech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Text-to-Speech API")
    (description "Go Client Library for Cloud Text-to-Speech API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-tpu
  (package
    (name "go-cloud-google-com-go-tpu")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "tpu"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/tpu"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud TPU API")
    (description "Go Client Library for Cloud TPU API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-trace
  (package
    (name "go-cloud-google-com-go-trace")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "trace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/trace"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Stackdriver Trace API")
    (description "Go Client Library for Stackdriver Trace API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-video
  (package
    (name "go-cloud-google-com-go-video")
    (version "1.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "video"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/video"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Video APIs")
    (description "Go Client Library for Video APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-videointelligence
  (package
    (name "go-cloud-google-com-go-videointelligence")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "videointelligence"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/videointelligence"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Video Intelligence API")
    (description "Go Client Library for Google Cloud Video Intelligence API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vision
  (package
    (name "go-cloud-google-com-go-vision")
    (version "2.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vision"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vision/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Vision API")
    (description "Go Client Library for Cloud Vision API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmmigration
  (package
    (name "go-cloud-google-com-go-vmmigration")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmmigration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vmmigration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VM Migration API")
    (description "Go Client Library for VM Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmwareengine
  (package
    (name "go-cloud-google-com-go-vmwareengine")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmwareengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vmwareengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VMware Engine API")
    (description "Go Client Library for VMware Engine API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vpcaccess
  (package
    (name "go-cloud-google-com-go-vpcaccess")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vpcaccess"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vpcaccess"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Serverless VPC Access API")
    (description "Go Client Library for Serverless VPC Access API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-webrisk
  (package
    (name "go-cloud-google-com-go-webrisk")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "webrisk"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/webrisk"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Risk API")
    (description "Go Client Library for Web Risk API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-websecurityscanner
  (package
    (name "go-cloud-google-com-go-websecurityscanner")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "websecurityscanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/websecurityscanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Security Scanner API")
    (description "Go Client Library for Web Security Scanner API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-translate
  (package
    (name "go-cloud-google-com-go-translate")
    (version "1.12.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "translate"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/translate"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-text
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Translation API")
    (description
     "Package translate is the v2 client for the Google Translation API.")
    (license license:asl2.0)))

(define-public go-github-com-google-s2a-go
  (package
    (name "go-github-com-google-s2a-go")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/s2a-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19d2n9w3lm08iiggj9nm4wh64czjbkis3kyvzsy6cqmlyjykch0v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/s2a-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-appengine
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-crypto
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-translate))
    (home-page "https://github.com/google/s2a-go")
    (synopsis "Secure Session Agent Client Libraries")
    (description
     "Package s2a provides the S2A transport credentials used by a @code{gRPC}
application.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-pkcs11
  (package
    (name "go-github-com-google-go-pkcs11")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-pkcs11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d93v3c0gsrymwagjfp9nzf70yxvczc6kvrqz10w2a07ag5ym02g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-pkcs11"))
    (home-page "https://github.com/google/go-pkcs11")
    (synopsis "Go PKCS #11")
    (description
     "This package provides a Go package for loading PKCS #11 modules.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-enterprise-certificate-proxy
  (package
    (name "go-github-com-googleapis-enterprise-certificate-proxy")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/enterprise-certificate-proxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01y66q708w2vp89gb10iy6vki86hssjwvhia2r0dvwdvbfb9rxi8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/enterprise-certificate-proxy"))
    (propagated-inputs (list go-golang-org-x-sys go-golang-org-x-crypto
                             go-github-com-google-go-pkcs11))
    (home-page "https://github.com/googleapis/enterprise-certificate-proxy")
    (synopsis "Google Proxies for Enterprise Certificates (GA)")
    (description
     "If you use
@@url{https://cloud.google.com/beyondcorp-enterprise/docs/securing-resources-with-certificate-based-access,certificate-based
access} to protect your Google Cloud resources, the end user
@@url{https://en.wikipedia.org/wiki/Client_certificate,device certificate} is
one of the credentials that is verified before access to a resource is granted.
You can configure Google Cloud to use the device certificates in your operating
system key store when verifying access to a resource from the gcloud CLI or
Terraform by using the enterprise certificates feature.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-auth
  (package
    (name "go-cloud-google-com-go-auth")
    (version "0.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1srq1a4vyqdr7arvj7ri0p2w3spz63jx46xn9gzcpycgxah7ihkb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/auth"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-golang-org-x-time
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Auth Library for Go")
    (description
     "Package auth provides utilities for managing Google Cloud credentials, including
functionality for creating, caching, and refreshing OAuth2 tokens.  It offers
customizable options for different OAuth2 flows, such as 2-legged (2LO) and
3-legged (3LO) OAuth, along with support for PKCE and automatic token
management.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iam
  (package
    (name "go-cloud-google-com-go-iam")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iam"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iam"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "IAM API")
    (description
     "Package iam supports the resource-specific operations of Google Cloud IAM
(Identity and Access Management) for the Google Cloud Libraries.  See
@@url{https://cloud.google.com/iam,https://cloud.google.com/iam} for more about
IAM.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-monitoring
  (package
    (name "go-cloud-google-com-go-monitoring")
    (version "1.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "monitoring"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/monitoring"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Monitoring API")
    (description "Go Client Library for Cloud Monitoring API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric")
    (version "0.52.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporter/metric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12924v4j592plz62zcq4crxajlm7cyf0ysmaf0mparwvl6b2x7m5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/exporter/metric"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-google-golang-org-genproto-googleapis-api
                             go-go-opentelemetry-io-otel-trace
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-monitoring))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "OpenTelemetry Google Cloud Monitoring Exporter")
    (description
     "@code{OpenTelemetry} Google Cloud Monitoring Exporter allows the user to send
collected metrics to Google Cloud.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12924v4j592plz62zcq4crxajlm7cyf0ysmaf0mparwvl6b2x7m5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/detectors/gcp"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-cloud-google-com-go-compute-metadata))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "GCP Resource detection library")
    (description
     "This is a library intended to be used by Upstream @code{OpenTelemetry} resource
detectors.  It exists within this repository to allow for integration testing of
the detection functions in real GCP environments.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-detectors-gcp
  (package
    (name "go-go-opentelemetry-io-contrib-detectors-gcp")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/contrib/detectors/gcp"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel
                        go-github-com-stretchr-testify
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis "GCP Resource detector")
    (description
     "Package gcp provides a resource detector for GCP Cloud Function.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdoutmetric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdoutmetric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Metric Exporter")
    (description
     "Package stdoutmetric provides an exporter for @code{OpenTelemetry} metric
telemetry.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-storage
  (package
    (name "go-cloud-google-com-go-storage")
    (version "1.55.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b2s6zcjckd4hmxlxfybfpi8virdn2siajffp7mcsfvxhj4xypdg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/storage"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Storage")
    (description
     "Package storage provides an easy way to work with Google Cloud Storage.  Google
Cloud Storage stores data in named objects, which are grouped into buckets.")
    (license license:asl2.0)))

(define-public go-github-com-google-martian
  (package
    (name "go-github-com-google-martian")
    (version "3.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/martian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0js95rw72mklxx8dilqdc86a50yhvykyczck4ci3xx6090p3fj2q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/martian/v3"
      #:unpack-path "github.com/google/martian"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc go-golang-org-x-net
                             go-github-com-golang-snappy))
    (home-page "https://github.com/google/martian")
    (synopsis "Martian Proxy")
    (description
     "Package martian provides an HTTP/1.1 proxy with an API for configurable request
and response modifiers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.121.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dl179xpx9xb0k3s23m49k1mw5krrl20cs6026g7rmz584b54nsh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-martian-v3
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Client Libraries for Go")
    (description
     "Package cloud is the root of the packages used to access Google Cloud Services.
See
@@url{https://pkg.go.dev/cloud.google.com/go#section-directories,https://pkg.go.dev/cloud.google.com/go#section-directories}
for a full list of sub-modules.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-longrunning
  (package
    (name "go-cloud-google-com-go-longrunning")
    (version "0.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "longrunning"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/longrunning"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "longrunning")
    (description
     "Package longrunning supports Long Running Operations for the Google Cloud
Libraries.  See google.golang.org/genproto/googleapis/longrunning for its
service definition.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-workflows
  (package
    (name "go-cloud-google-com-go-workflows")
    (version "1.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "workflows"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/workflows"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Workflows API")
    (description "Go Client Library for Workflows API.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-workflows
                             go-cloud-google-com-go-websecurityscanner
                             go-cloud-google-com-go-webrisk
                             go-cloud-google-com-go-vpcaccess
                             go-cloud-google-com-go-vmwareengine
                             go-cloud-google-com-go-vmmigration
                             go-cloud-google-com-go-vision-v2
                             go-cloud-google-com-go-videointelligence
                             go-cloud-google-com-go-video
                             go-cloud-google-com-go-translate
                             go-cloud-google-com-go-trace
                             go-cloud-google-com-go-tpu
                             go-cloud-google-com-go-texttospeech
                             go-cloud-google-com-go-talent
                             go-cloud-google-com-go-storagetransfer
                             go-cloud-google-com-go-speech
                             go-cloud-google-com-go-spanner
                             go-cloud-google-com-go-shell
                             go-cloud-google-com-go-servicedirectory
                             go-cloud-google-com-go-securitycenter
                             go-cloud-google-com-go-security
                             go-cloud-google-com-go-secretmanager
                             go-cloud-google-com-go-scheduler
                             go-cloud-google-com-go-run
                             go-cloud-google-com-go-retail
                             go-cloud-google-com-go-resourcesettings
                             go-cloud-google-com-go-resourcemanager
                             go-cloud-google-com-go-redis
                             go-cloud-google-com-go-recommender
                             go-cloud-google-com-go-recommendationengine
                             go-cloud-google-com-go-recaptchaenterprise-v2
                             go-cloud-google-com-go-pubsublite
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-privatecatalog
                             go-cloud-google-com-go-policytroubleshooter
                             go-cloud-google-com-go-phishingprotection
                             go-cloud-google-com-go-oslogin
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-orchestration
                             go-cloud-google-com-go-optimization
                             go-cloud-google-com-go-notebooks
                             go-cloud-google-com-go-networksecurity
                             go-cloud-google-com-go-networkmanagement
                             go-cloud-google-com-go-networkconnectivity
                             go-cloud-google-com-go-monitoring
                             go-cloud-google-com-go-metastore
                             go-cloud-google-com-go-memcache
                             go-cloud-google-com-go-mediatranslation
                             go-cloud-google-com-go-maps
                             go-cloud-google-com-go-managedidentities
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-logging
                             go-cloud-google-com-go-lifesciences
                             go-cloud-google-com-go-language
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iot
                             go-cloud-google-com-go-ids
                             go-cloud-google-com-go-iap
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-gsuiteaddons
                             go-cloud-google-com-go-gkemulticloud
                             go-cloud-google-com-go-gkehub
                             go-cloud-google-com-go-gkeconnect
                             go-cloud-google-com-go-gkebackup
                             go-cloud-google-com-go-functions
                             go-cloud-google-com-go-firestore
                             go-cloud-google-com-go-filestore
                             go-cloud-google-com-go-eventarc
                             go-cloud-google-com-go-essentialcontacts
                             go-cloud-google-com-go-errorreporting
                             go-cloud-google-com-go-edgecontainer
                             go-cloud-google-com-go-domains
                             go-cloud-google-com-go-documentai
                             go-cloud-google-com-go-dlp
                             go-cloud-google-com-go-dialogflow
                             go-cloud-google-com-go-deploy
                             go-cloud-google-com-go-datastream
                             go-cloud-google-com-go-datastore
                             go-cloud-google-com-go-dataqna
                             go-cloud-google-com-go-dataproc-v2
                             go-cloud-google-com-go-dataplex
                             go-cloud-google-com-go-datalabeling
                             go-cloud-google-com-go-datafusion
                             go-cloud-google-com-go-dataform
                             go-cloud-google-com-go-dataflow
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go-containeranalysis
                             go-cloud-google-com-go-container
                             go-cloud-google-com-go-contactcenterinsights
                             go-cloud-google-com-go-compute
                             go-cloud-google-com-go-cloudtasks
                             go-cloud-google-com-go-clouddms
                             go-cloud-google-com-go-cloudbuild
                             go-cloud-google-com-go-channel
                             go-cloud-google-com-go-certificatemanager
                             go-cloud-google-com-go-binaryauthorization
                             go-cloud-google-com-go-billing
                             go-cloud-google-com-go-bigtable
                             go-cloud-google-com-go-bigquery
                             go-cloud-google-com-go-beyondcorp
                             go-cloud-google-com-go-batch
                             go-cloud-google-com-go-baremetalsolution
                             go-cloud-google-com-go-automl
                             go-cloud-google-com-go-assuredworkloads
                             go-cloud-google-com-go-asset
                             go-cloud-google-com-go-artifactregistry
                             go-cloud-google-com-go-area120
                             go-cloud-google-com-go-appengine
                             go-cloud-google-com-go-apigeeregistry
                             go-cloud-google-com-go-apigeeconnect
                             go-cloud-google-com-go-apigateway
                             go-cloud-google-com-go-analytics
                             go-cloud-google-com-go-aiplatform
                             go-cloud-google-com-go-accesscontextmanager
                             go-cloud-google-com-go-accessapproval))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
     "This repository contains the generated Go packages for common protocol buffer
types, and the generated @@url{http://grpc.io,@code{gRPC}} code necessary for
interacting with Google's @code{gRPC} APIs.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto-googleapis-api
  (package
    (name "go-google-golang-org-genproto-googleapis-api")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto/googleapis/api"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-googleapis-gax-go
  (package
    (name "go-github-com-googleapis-gax-go")
    (version "2.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/gax-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10lx4d7bw2a9j5ymjwjbn4jnvqmg97p6hjnrdmjwpcgapq2yfmad"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/gax-go/v2"
      #:unpack-path "github.com/googleapis/gax-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/googleapis/gax-go")
    (synopsis #f)
    (description
     "Package gax contains a set of modules which aid the development of APIs for
clients and servers based on @code{gRPC} and Google API conventions.")
    (license license:bsd-3)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
  (package
    (name
     "go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc")
    (version "0.61.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                      #:subdir
                      "instrumentation/google.golang.org/grpc/otelgrpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/google.golang.org/grpc/otelgrpc"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description "Package otelgrpc is the instrumentation library for
@@url{/google.golang.org/grpc,google.golang.org/grpc}.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto-googleapis-bytestream
  (package
    (name "go-google-golang-org-genproto-googleapis-bytestream")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/bytestream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto/googleapis/bytestream"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-api
  (package
    (name "go-google-golang-org-api")
    (version "0.237.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-api-go-client")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ydz4v4j3hwi9745i7l9mac6cjy17xpvir28j7p2l5d08r0jbxrm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/api"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-bytestream
                        go-golang-org-x-time
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-uuid
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth-oauth2adapt
                        go-cloud-google-com-go-auth))
    (home-page "https://google.golang.org/api")
    (synopsis "Google APIs Client Library for Go")
    (description
     "Package api is the root of the packages used to access Google Cloud Services.
See
@@url{https://godoc.org/google.golang.org/api,https://godoc.org/google.golang.org/api}
for a full list of sub-packages.")
    (license license:bsd-3)))

(define-public go-github-com-googleapis-go-sql-spanner
  (package
    (name "go-github-com-googleapis-go-sql-spanner")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-sql-spanner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12s2ws7kf09lvl6zyicba288mpv85igh1lia8p9w7vvbaqyk57nz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:import-path "github.com/googleapis/go-sql-spanner"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-api
                             go-github-com-hashicorp-golang-lru
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-spanner
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://github.com/googleapis/go-sql-spanner")
    (synopsis "go-sql-spanner")
    (description
     "@@url{https://cloud.google.com/spanner,Google Cloud Spanner} driver for Go's
@@url{https://golang.org/pkg/database/sql/,database/sql} package.")
    (license license:asl2.0)))

(define-public go-github-com-go-testfixtures-testfixtures
  (package
    (name "go-github-com-go-testfixtures-testfixtures")
    (version "3.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-testfixtures/testfixtures")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "020smlvia49a7cb04f69sc5jkyhnihr67jy3mh0dwv23lk20vhfg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/go-testfixtures/testfixtures/v3"
      #:unpack-path "github.com/go-testfixtures/testfixtures"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-spf13-pflag
                             go-github-com-mattn-go-sqlite3
                             go-github-com-lib-pq
                             go-github-com-joho-godotenv
                             go-github-com-jackc-pgx-v4
                             go-github-com-googleapis-go-sql-spanner
                             go-github-com-goccy-go-yaml
                             go-github-com-go-sql-driver-mysql
                             go-github-com-denisenkom-go-mssqldb
                             go-github-com-clickhouse-clickhouse-go-v2
                             go-cloud-google-com-go-spanner))
    (home-page "https://github.com/go-testfixtures/testfixtures")
    (synopsis "testfixtures")
    (description
     "Writing tests is hard, even more when you have to deal with an SQL database.
This package aims to make writing functional tests for web apps written in Go
easier.")
    (license license:expat)))

(define-public go-github-com-gogs-cron
  (package
    (name "go-github-com-gogs-cron")
    (version "0.0.0-20171120032916-9f6c956d3e14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gogs/cron")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06nxf8c5rxjjzprpdyiq2pyhckqhgn6ad22hmrxmzyd7z6y34xcj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gogs/cron"))
    (home-page "https://github.com/gogs/cron")
    (synopsis #f)
    (description "Package cron implements a cron spec parser and job runner.")
    (license license:expat)))

(define-public go-github-com-gogs-go-gogs-client
  (package
    (name "go-github-com-gogs-go-gogs-client")
    (version "0.0.0-20210131175652-1d7215cd8d85")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gogs/go-gogs-client")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11b8mwdly960z7jdx297jywxzljs0l0sl3i1qvii1fjzis7k608l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gogs/go-gogs-client"))
    (home-page "https://github.com/gogs/go-gogs-client")
    (synopsis "Gogs API client in Go")
    (description
     "This package is still in experiment, see
@@url{https://github.com/gogits/go-gogs-client/wiki,Wiki} for documentation.")
    (license license:expat)))

(define-public go-github-com-google-go-github
  (package
    (name "go-github-com-google-go-github")
    (version "52.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-github")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00xkzaap5awwlmm67cpcgah1qfnjd65vzhwi2vszs9n17r4hp7dm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-github/v52"
      #:unpack-path "github.com/google/go-github"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-google-go-querystring
                             go-github-com-google-go-cmp
                             go-github-com-protonmail-go-crypto))
    (home-page "https://github.com/google/go-github")
    (synopsis "go-github")
    (description "go-github is a Go client library for accessing the
@@url{https://docs.github.com/en/rest,@code{GitHub} API v3}.")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-feeds
  (package
    (name "go-github-com-gorilla-feeds")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/feeds")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0285iq589q7i6mr2wl3qzr282yc82sg1raxfna0xxvin7a5k7mx6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gorilla/feeds"))
    (propagated-inputs (list go-github-com-kr-pretty))
    (home-page "https://github.com/gorilla/feeds")
    (synopsis "gorilla/feeds")
    (description "Syndication (feed) generator library for golang.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-crypto
  (package
    (name "go-github-com-keybase-go-crypto")
    (version "0.0.0-20200123153347-de78d2cb44f4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/go-crypto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0prrpv0x7nbq5k6swn2jwypzxa8h4aj5lgyw372n6c8ln34fh9jq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-crypto"))
    (home-page "https://github.com/keybase/go-crypto")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-gorilla-pat
  (package
    (name "go-github-com-gorilla-pat")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gorilla/pat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ip2mfhs04j1wbh69iq6d3xz760cb6qmxgv3csns6qrkxfr53av2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gorilla/pat"))
    (propagated-inputs (list go-github-com-gorilla-mux
                             go-github-com-gorilla-context))
    (home-page "https://github.com/gorilla/pat")
    (synopsis "pat")
    (description
     "Package gorilla/pat is a request router and dispatcher with a pat-like
interface.  It is an alternative to gorilla/mux that showcases how it can be
used as a base for different API flavors.  Package pat is documented at:.")
    (license license:bsd-3)))

(define-public go-github-com-lestrrat-go-backoff
  (package
    (name "go-github-com-lestrrat-go-backoff")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/backoff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s939szsdv0ggp69rig8dkl74s5dvwzm5cw80h0b3dvkqhikim5d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/backoff/v2"
      #:unpack-path "github.com/lestrrat-go/backoff"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-lestrrat-go-option))
    (home-page "https://github.com/lestrrat-go/backoff")
    (synopsis "backoff")
    (description
     "Package backoff implments backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-blackmagic
  (package
    (name "go-github-com-lestrrat-go-blackmagic")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/blackmagic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyij1wnsh85vqi70sq0kgwrnx4zrn4yx8nk5lqd630g1akqwr8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/lestrrat-go/blackmagic"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/blackmagic")
    (synopsis "blackmagic")
    (description "Reflect-based black magic.  YMMV, and use with caution.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-httpcc
  (package
    (name "go-github-com-lestrrat-go-httpcc")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/httpcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wsr6ipl3h7iaq7s7a2mgkbli9z5zpxj9dxqhzqn33akb055i28"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/httpcc"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/httpcc")
    (synopsis "httpcc")
    (description
     "Parses HTTP/1.1 Cache-Control header, and returns a struct that is convenient
for the end-user to do what they will with.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-iter
  (package
    (name "go-github-com-lestrrat-go-iter")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/iter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p5fhw5g3kh7c6hvw2mc1r4ckxb3ax262x8b736yyhpv2ynl8jyz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/iter"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/iter")
    (synopsis "iter")
    (description "Simple tools for container iteration.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-option
  (package
    (name "go-github-com-lestrrat-go-option")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/option")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p9744hpdxsnimha5i0gyn7hxn2fy3dxqhlpqvj5s3pc5xv3s14h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/option"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/option")
    (synopsis "option")
    (description "Base object for the \"Optional Parameters Pattern\".")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-jwx
  (package
    (name "go-github-com-lestrrat-go-jwx")
    (version "1.2.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/jwx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ljbnw7fd9d5xggixrkx7fg9gs4jk23m6xkfy7s1rc7ljkh1n1qk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/jwx"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-pkg-errors
                             go-github-com-lestrrat-go-option
                             go-github-com-lestrrat-go-iter
                             go-github-com-lestrrat-go-httpcc
                             go-github-com-lestrrat-go-blackmagic
                             go-github-com-lestrrat-go-backoff-v2
                             go-github-com-goccy-go-json
                             go-github-com-decred-dcrd-dcrec-secp256k1-v4))
    (home-page "https://github.com/lestrrat-go/jwx")
    (synopsis "github.com/lestrrat-go/jwx")
    (description
     "Package jwx contains tools that deal with the various JWx (JOSE) technologies
such as JWT, JWS, JWE, etc in Go.")
    (license license:expat)))

(define-public go-github-com-serenize-snaker
  (package
    (name "go-github-com-serenize-snaker")
    (version "0.0.0-20201027110005-a7ad2135616e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/serenize/snaker")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c1d335q1i3mz55bhs2k84rcrz4xdaps2y63vwkyv9fsjpb2wnzb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/serenize/snaker"))
    (home-page "https://github.com/serenize/snaker")
    (synopsis "snaker")
    (description
     "Package snaker provides methods to convert @code{CamelCase} names to snake_case
and back.  It considers the list of allowed initialsms used by
github.com/golang/lint/golint (e.g. ID or HTTP).")
    (license license:expat)))

(define-public go-github-com-markbates-going
  (package
    (name "go-github-com-markbates-going")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/markbates/going")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14b8mj3x7bynpdw99a260j6gpjsqnki5fh3bbsf9cyggsgai11zz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/markbates/going"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-serenize-snaker))
    (home-page "https://github.com/markbates/going")
    (synopsis "github.com/markbates/going")
    (description
     "This project houses, what I consider to be some help packages for writing Go
applications.  Your mileage may vary, but I find them to be pretty darn helpful.")
    (license license:expat)))

(define-public go-github-com-mrjones-oauth
  (package
    (name "go-github-com-mrjones-oauth")
    (version "0.0.0-20190623134757-126b35219450")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrjones/oauth")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "120ajfcqcbdiglxl31yq1xgp6ahpb7jfnyk0587b9mljkaggs8gh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mrjones/oauth"))
    (home-page "https://github.com/mrjones/oauth")
    (synopsis "OAuth 1.0 Library for")
    (description "OAuth 1.0 consumer implementation.  See
@@url{http://www.oauth.net,http://www.oauth.net} and
@@url{https://rfc-editor.org/rfc/rfc5849.html,RFC 5849}.")
    (license license:expat)))

(define-public go-github-com-markbates-goth
  (package
    (name "go-github-com-markbates-goth")
    (version "1.81.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/markbates/goth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bl1l2ch1hj2mxz6jqfx2iik73d5qi5znbyf7h37bnxfpk1q32zs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/markbates/goth"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-stretchr-testify
                             go-github-com-mrjones-oauth
                             go-github-com-markbates-going
                             go-github-com-lestrrat-go-jwx
                             go-github-com-jarcoal-httpmock
                             go-github-com-gorilla-sessions
                             go-github-com-gorilla-pat
                             go-github-com-gorilla-mux
                             go-github-com-golang-jwt-jwt-v5
                             go-github-com-go-chi-chi-v5))
    (home-page "https://github.com/markbates/goth")
    (synopsis "Goth: Multi-Provider Authentication for Go")
    (description
     "Package goth provides a simple, clean, and idiomatic way to write authentication
packages for Go web applications.")
    (license license:expat)))

(define-public go-github-com-meilisearch-meilisearch-go
  (package
    (name "go-github-com-meilisearch-meilisearch-go")
    (version "0.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/meilisearch/meilisearch-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00b0pxy12996h2slm7ppbjahgwz88z1drp3y9hkh4nrwiyvkdmb1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/meilisearch/meilisearch-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-mailru-easyjson
                             go-github-com-golang-jwt-jwt-v4
                             go-github-com-andybalholm-brotli))
    (home-page "https://github.com/meilisearch/meilisearch-go")
    (synopsis "Table of Contents")
    (description
     "Package meilisearch is the official Meilisearch SDK for the Go programming
language.")
    (license license:expat)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "0.0.0-20180221191011-83c6a9932646")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nfnt/resize")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "005cpiwq28krbjf0zjwpfh63rp4s4is58700idn24fs3g7wdbwya"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/nfnt/resize"))
    (home-page "https://github.com/nfnt/resize")
    (synopsis
     "This package is no longer being updated! Please look for alternatives if that bothers you.")
    (description "Package resize implements various image resizing methods.")
    (license license:isc)))

(define-public go-github-com-oliamb-cutter
  (package
    (name "go-github-com-oliamb-cutter")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oliamb/cutter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cp7449pqg8wkcpcaqqq3rrmxgd88kk4pwh32hx1k4xdlvwm8ffr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/oliamb/cutter"))
    (home-page "https://github.com/oliamb/cutter")
    (synopsis "Cutter")
    (description "Package cutter provides a function to crop image.")
    (license license:expat)))

(define-public go-github-com-fortytw2-leaktest
  (package
    (name "go-github-com-fortytw2-leaktest")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fortytw2/leaktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0487zghyxqzk6zdbhd2j074pcc2l15l4sfg5clrjqwfbql7519wx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fortytw2/leaktest"))
    (home-page "https://github.com/fortytw2/leaktest")
    (synopsis "Leaktest")
    (description
     "Package leaktest provides tools to detect leaked goroutines in tests.  To use
it, call \"defer @code{leaktest.Check(t)()}\" at the beginning of each test that
may use goroutines.  copied out of the cockroachdb source tree with slight
modifications to be more re-useable.")
    (license license:bsd-3)))

(define-public go-github-com-smartystreets-go-aws-auth
  (package
    (name "go-github-com-smartystreets-go-aws-auth")
    (version "0.0.0-20180515143844-0c1422d1fdb9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smarty-archives/go-aws-auth")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0krfdpgn3gfii1z9fi8ydfw0wwfqyvp6w3rji7w92m528zkjl93d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smartystreets/go-aws-auth"))
    (home-page "https://github.com/smartystreets/go-aws-auth")
    (synopsis "go-aws-auth")
    (description
     "Package awsauth implements AWS request signing using Signed Signature Version 2,
Signed Signature Version 3, and Signed Signature Version 4.  Supports S3 and
STS.")
    (license license:expat)))

(define-public go-github-com-olivere-elastic
  (package
    (name "go-github-com-olivere-elastic")
    (version "7.0.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olivere/elastic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wgb891qs4rgw1sfpwfnplp57g4w4sq3ci31h7121cx118hy1v17"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/olivere/elastic/v7"
      #:unpack-path "github.com/olivere/elastic"))
    (propagated-inputs (list go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-github-com-smartystreets-go-aws-auth
                             go-github-com-pkg-errors
                             go-github-com-opentracing-opentracing-go
                             go-github-com-mailru-easyjson
                             go-github-com-google-go-cmp
                             go-github-com-fortytw2-leaktest
                             go-github-com-aws-aws-sdk-go))
    (home-page "https://github.com/olivere/elastic")
    (synopsis "Elastic")
    (description
     "Package elastic provides an interface to the Elasticsearch server
(@@url{https://www.elastic.co/products/elasticsearch,https://www.elastic.co/products/elasticsearch}).")
    (license license:expat)))

(define-public go-github-com-pquerna-otp
  (package
    (name "go-github-com-pquerna-otp")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pquerna/otp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0krrardi923jc39lar4vpbvdg3382x1wzk1ip3hwjc8f4jydqgai"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pquerna/otp"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-boombuler-barcode))
    (home-page "https://github.com/pquerna/otp")
    (synopsis "otp: One Time Password utilities Go / Golang")
    (description
     "Package otp implements both HOTP and TOTP based one time passcodes in a Google
Authenticator compatible manner.")
    (license license:asl2.0)))

(define-public go-github-com-quasoft-websspi
  (package
    (name "go-github-com-quasoft-websspi")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quasoft/websspi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xgq9cghrf25zqjrx7hmc2qcjgh38lpkxb8b5i71c7k1nv1hh65z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quasoft/websspi"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-gorilla-sessions
                             go-github-com-gorilla-securecookie))
    (home-page "https://github.com/quasoft/websspi")
    (synopsis "websspi")
    (description
     "@@code{websspi} is an HTTP middleware for Golang that uses Kerberos/NTLM for
single sign-on (SSO) authentication of browser based clients in a Windows
environment.")
    (license license:expat)))

(define-public go-github-com-santhosh-tekuri-jsonschema
  (package
    (name "go-github-com-santhosh-tekuri-jsonschema")
    (version "5.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/santhosh-tekuri/jsonschema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fjklx6sik1pp4qpmzsvwfmavd1m9pcsyap1wvajhm2d8wx3vnh0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/santhosh-tekuri/jsonschema/v5"
      #:unpack-path "github.com/santhosh-tekuri/jsonschema"))
    (home-page "https://github.com/santhosh-tekuri/jsonschema")
    (synopsis "jsonschema v5.3.1")
    (description
     "Package jsonschema provides json-schema compilation and validation.")
    (license license:asl2.0)))

(define-public go-github-com-sassoftware-go-rpmutils
  (package
    (name "go-github-com-sassoftware-go-rpmutils")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sassoftware/go-rpmutils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06ymwjn6xvc4cpxcsh5achwgma4i075ikbzq8jm143m0pck4pmfi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sassoftware/go-rpmutils"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-go-uber-org-goleak
                             go-github-com-xi2-xz
                             go-github-com-ulikunitz-xz
                             go-github-com-stretchr-testify
                             go-github-com-klauspost-compress
                             go-github-com-protonmail-go-crypto
                             go-github-com-datadog-zstd))
    (home-page "https://github.com/sassoftware/go-rpmutils")
    (synopsis "Go RPM Utils")
    (description
     "go-rpmutils is a library written in @@url{http://golang.org,go} for parsing and
extracting content from @@url{http://www.rpm.org,RPMs}.")
    (license license:asl2.0)))

(define-public go-github-com-tstranex-u2f
  (package
    (name "go-github-com-tstranex-u2f")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tstranex/u2f")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xgyxmi8amlx35f23ldlkn900cyic77r525wpk5s58cpyw3hn5cd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tstranex/u2f"))
    (home-page "https://github.com/tstranex/u2f")
    (synopsis "Go FIDO U2F Library")
    (description
     "Package u2f implements the server-side parts of the FIDO Universal 2nd Factor
(U2F) specification.")
    (license license:expat)))

(define-public go-github-com-yohcop-openid-go
  (package
    (name "go-github-com-yohcop-openid-go")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yohcop/openid-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dkxsivwlv7zmk44iwhphrqx2fv2sighlf6vz451m9lap47851s9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yohcop/openid-go"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/yohcop/openid-go")
    (synopsis "openid.go")
    (description
     "This is a consumer (Relying party) implementation of @code{OpenId} 2.0, written
in Go.")
    (license license:asl2.0)))

(define-public go-github-com-yuin-goldmark-highlighting
  (package
    (name "go-github-com-yuin-goldmark-highlighting")
    (version "2.0.0-20230729083705-37449abec8cc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-highlighting")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "163jbi8si9qxcabzni39qypj495y48rabkkchc03ay68p19v160y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark-highlighting/v2"
      #:unpack-path "github.com/yuin/goldmark-highlighting"))
    (propagated-inputs (list go-github-com-yuin-goldmark
                             go-github-com-alecthomas-chroma-v2))
    (home-page "https://github.com/yuin/goldmark-highlighting")
    (synopsis "goldmark-highlighting")
    (description
     "package highlighting is a extension for the
goldmark(@@url{http://github.com/yuin/goldmark,http://github.com/yuin/goldmark}).")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-meta
  (package
    (name "go-github-com-yuin-goldmark-meta")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-meta")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07dnwpkcifk9lw25ncflwdzmp8xqwbsbq0bnw3v7ljz9i8zi3ya3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark-meta"))
    (propagated-inputs (list go-gopkg-in-yaml-v2 go-github-com-yuin-goldmark))
    (home-page "https://github.com/yuin/goldmark-meta")
    (synopsis "goldmark-meta")
    (description
     "package meta is a extension for the
goldmark(@@url{http://github.com/yuin/goldmark,http://github.com/yuin/goldmark}).")
    (license license:expat)))

(define-public go-strk-kbt-io-projects-go-libravatar
  (package
    (name "go-strk-kbt-io-projects-go-libravatar")
    (version "0.0.0-20191008002943-06d1c002b251")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://strk.kbt.io/git/go-libravatar.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s5q8iyx0sy5fav7qbivmza755pv5yjjjqh51vhs03pcxcz8fbi1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "strk.kbt.io/projects/go/libravatar"))
    (home-page "https://strk.kbt.io/projects/go/libravatar")
    (synopsis "Use")
    (description
     "Simple @@url{https://www.golang.org,golang} library for serving
@@url{https://www.libravatar.org,federated avatars}.")
    (license license:expat)))

(define-public go-gitee-com-travelliu-dm
  (package
    (name "go-gitee-com-travelliu-dm")
    (version "1.8.11192")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitee.com/travelliu/dm.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "077jwlqyrvnkbmrj8wdfw90q3r5vmz1p12m3s9lrnj15gb8aqn6d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitee.com/travelliu/dm"))
    (propagated-inputs (list go-golang-org-x-text go-github-com-golang-snappy))
    (home-page "https://gitee.com/travelliu/dm")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azcore
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azcore")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azcore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "135h8qdb2yjgd51xbkk50d0nh0w2pmi6v35dkjzdk73ci30kl8ky"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azcore"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-internal))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Core Client Module for Go")
    (description
     "Package azcore implements an HTTP request/response middleware pipeline used by
Azure SDK clients.")
    (license license:expat)))

(define-public go-github-com-keybase-go-keychain
  (package
    (name "go-github-com-keybase-go-keychain")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/go-keychain")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gkd839h8xnfiv0g52hm4p9snrcfgrnczrqf5wxr61sgg2w8h3y1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-keychain"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-keybase-dbus))
    (home-page "https://github.com/keybase/go-keychain")
    (synopsis "Go Keychain")
    (description
     "This package provides a library for accessing the Keychain for @code{macOS},
@code{iOS}, and Linux in Go (golang).")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
  (package
    (name
     "go-github-com-azuread-microsoft-authentication-extensions-for-go-cache")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jj7bjdns1dsxsk22p0xdph3j9hhysbwcbxwsqpxyyggzni9zijv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go/cache"
      #:unpack-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go"))
    (propagated-inputs (list go-gopkg-in-check-v1 go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-keybase-go-keychain
                        go-github-com-azuread-microsoft-authentication-library-for-go))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) Extensions for Go")
    (description
     "This module contains a persistent cache for
@@url{https://github.com/@code{AzureAD/microsoft-authentication-library-for-go,Microsoft}
Authentication Library (MSAL) for Go} public client applications such as CLI
tools.  It isn't recommended for web applications or RPC APIs, in which it can
cause scaling and performance problems.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity/cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x4bb3bgy4pgz67nwjjxiil4q9cmnjhnl8zh7vcrxirbwprpchja"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity/cache"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-google-uuid
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Cache Module for Go")
    (description
     "This module implements a cross-platform persistent token cache for
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity,azidentity}
credentials.  See that module's
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity#pkg-examples,examples}
for sample code showing how to configure persistent caching for a credential,
and its @@url{https://aka.ms/azsdk/go/identity/caching,token caching document}
for more information about the implementation.")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-library-for-go
  (package
    (name "go-github-com-azuread-microsoft-authentication-library-for-go")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-library-for-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17sc8d4xajp9ni1m5vbiwajvh0siay3lmssm24hydzmalljrc3pd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-library-for-go"))
    (propagated-inputs (list go-github-com-pkg-browser
                             go-github-com-montanaflynn-stats
                             go-github-com-kylelemons-godebug
                             go-github-com-google-uuid
                             go-github-com-golang-jwt-jwt-v5))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-library-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) for Go")
    (description
     "The Microsoft Authentication Library (MSAL) for Go is part of the
@@url{https://aka.ms/aaddevv2,Microsoft identity platform for developers}
(formerly named Azure AD) v2.0.  It allows you to sign in users or apps with
Microsoft identities
(@@url{https://azure.microsoft.com/services/active-directory/,Azure AD} and
@@url{https://account.microsoft.com,Microsoft Accounts}) and obtain tokens to
call Microsoft APIs such as @@url{https://graph.microsoft.io/,Microsoft Graph}
or your own APIs registered with the Microsoft identity platform.  It is built
using industry standard OAuth2 and @code{OpenID} Connect protocols.")
    (license license:expat)))

(define-public go-github-com-bsm-ginkgo
  (package
    (name "go-github-com-bsm-ginkgo")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01k1j1bwdq23hs9zzbz9kdljvr6hzym53mqxh2gy0bz4lggcd6qs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/ginkgo/v2"
      #:unpack-path "github.com/bsm/ginkgo"))
    (home-page "https://github.com/bsm/ginkgo")
    (synopsis "Ginkgo")
    (description
     "Ginkgo is a testing framework for Go designed to help you write expressive
tests. @@url{https://github.com/onsi/ginkgo,https://github.com/onsi/ginkgo}
MIT-Licensed.")
    (license license:expat)))

(define-public go-github-com-bsm-gomega
  (package
    (name "go-github-com-bsm-gomega")
    (version "1.27.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7p85wsqv1j9aq052vdw006xq42n1rdgnk1lr6f5wnapwab2shz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/gomega"))
    (home-page "https://github.com/bsm/gomega")
    (synopsis "Gomega")
    (description
     "Gomega is the Ginkgo BDD-style testing framework's preferred matcher library.")
    (license license:expat)))

(define-public go-github-com-redis-go-redis
  (package
    (name "go-github-com-redis-go-redis")
    (version "9.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y4zd09yhbhr07idcny95ilh57vcva5kcsnnx2ffa3w4k2c33181"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/redis/go-redis/v9"
      #:unpack-path "github.com/redis/go-redis"))
    (propagated-inputs (list go-github-com-dgryski-go-rendezvous
                             go-github-com-cespare-xxhash-v2
                             go-github-com-bsm-gomega
                             go-github-com-bsm-ginkgo-v2))
    (home-page "https://github.com/redis/go-redis")
    (synopsis "Redis client for Go")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8zkxnrpadp62p9x6kgd9nfbnmppvgmvf2li78y590xykppdcds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-crypto
                        go-github-com-stretchr-testify
                        go-github-com-redis-go-redis-v9
                        go-github-com-google-uuid
                        go-github-com-golang-jwt-jwt-v5
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Client Module for Go")
    (description
     "The Azure Identity module provides Microsoft Entra ID
(@@url{https://learn.microsoft.com/entra/fundamentals/new-name,formerly Azure
Active Directory}) token authentication support across the Azure SDK. It
includes a set of @@code{@code{TokenCredential}} implementations, which can be
used with Azure SDK clients supporting token authentication.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-internal
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-internal")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0czvydkb9sgk2sy1dwl78rbd81gjy5ykhpasrm24h4hhv382nyhz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/internal"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-text go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure.Core Internal Module for Go")
    (description "internal contains content for Azure SDK developers.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/security/keyvault/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16xb2gyl48xcc1xc58l8c8qhraxsmar7655348ggi4r2jzyc6sik"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/security/keyvault/internal"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Key Vault Internal Module for Go")
    (description
     "This module contains shared code for all the Key Vault SDKs, mainly the
challenge authentication policy.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/security/keyvault/azkeys"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jxpzd6sg6fp2mim3sr7gg9y0lvlngf23mmij388ywz4zfvpcqhq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/security/keyvault/azkeys"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Key Vault Keys client module for Go")
    (description
     "@@url{https://github.com/Azure/azure-sdk-for-go/tree/main/sdk/security/keyvault/azkeys/client.go,Source
code} | @@url{https://aka.ms/azsdk/go/keyvault-keys/docs,Package (pkg.go.dev)} |
@@url{https://learn.microsoft.com/azure/key-vault/,Product documentation} |
@@url{https://aka.ms/azsdk/go/keyvault-keys/docs#pkg-examples,Samples}.")
    (license license:expat)))

(define-public go-github-com-golang-sql-civil
  (package
    (name "go-github-com-golang-sql-civil")
    (version "0.0.0-20220223132316-b832511892a9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-sql/civil")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "152smf33psdad1222jrabpkl7yvkzw8k66hyypn5gj07943gsk10"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang-sql/civil"))
    (home-page "https://github.com/golang-sql/civil")
    (synopsis "Civil Date and Time")
    (description
     "Package civil implements types for civil time, a time-zone-independent
representation of time that follows the rules of the proleptic Gregorian
calendar with exactly 24-hour days, 60-minute hours, and 60-second minutes.")
    (license license:asl2.0)))

(define-public go-github-com-golang-sql-sqlexp
  (package
    (name "go-github-com-golang-sql-sqlexp")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-sql/sqlexp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g7apf8mfzrzh1dzkhskbiabb8xc6ghbnxnc4bdk9hzgvrv9c4hn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang-sql/sqlexp"))
    (home-page "https://github.com/golang-sql/sqlexp")
    (synopsis "golang-sql exp")
    (description
     "Package sqlexp provides interfaces and functions that may be adopted into the
database/sql package in the future.  All features may change or be removed in
the future.")
    (license license:bsd-3)))

(define-public go-github-com-microsoft-go-mssqldb
  (package
    (name "go-github-com-microsoft-go-mssqldb")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/go-mssqldb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z7w53dd705d5y6a01ldrnixxgbzlmv7rwpgk8103j7p5vg797gi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/microsoft/go-mssqldb"))
    (propagated-inputs (list go-golang-org-x-text
                        go-golang-org-x-sys
                        go-golang-org-x-crypto
                        go-github-com-stretchr-testify
                        go-github-com-jcmturner-gokrb5-v8
                        go-github-com-google-uuid
                        go-github-com-golang-sql-sqlexp
                        go-github-com-golang-sql-civil
                        go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/microsoft/go-mssqldb")
    (synopsis "Microsoft's official Go MSSQL driver")
    (description
     "package mssql implements the TDS protocol used to connect to MS SQL Server
(sqlserver) database servers.")
    (license license:bsd-3)))

(define-public go-github-com-ziutek-mymysql
  (package
    (name "go-github-com-ziutek-mymysql")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ziutek/mymysql")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "172s7sv5bgc40x81k18hypf9c4n8hn9v5w5zwyr4mi5prbavqcci"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ziutek/mymysql"))
    (home-page "https://github.com/ziutek/mymysql")
    (synopsis "MyMySQL v1.5.4 (2015-01-08)")
    (description
     "Sorry for my poor English.  If you can help with improving the English in this
documentation, please contact me.")
    (license license:bsd-3)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "4.26.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j3j9w3p66wgi0ckiw8rvaahqxcp5ai1ndikxpcx7qb9mv2m3qp6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v4"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-sortutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-ccorpus2
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v4")
    (description "Package cc is a C99 compiler front end.")
    (license license:bsd-3)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "3.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khw9qsaz4ab0vb4kazgfm481cjpcyxj6ld2ma4d9hva3ca9h8ji"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v3"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-mathutil
                             go-lukechampine-com-uint128
                             go-github-com-google-go-cmp
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccorpus
  (package
    (name "go-modernc-org-ccorpus")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccorpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18d5npw8aw5qzy6qcrlrili2zxvmc2v4kkwjps6c3ayvi7aj7j09"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccorpus"))
    (propagated-inputs (list go-modernc-org-httpfs))
    (home-page "https://modernc.org/ccorpus")
    (synopsis "ccorpus")
    (description "Package ccorpus provides a test corpus of C code.")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "3.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkxzhmkm5pgalcnlhw0whiii6vhdpnnnpwkx9b5ah21ajk6qqlc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccgo/v3"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-ccorpus
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v3
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-github-com-pmezard-go-difflib
                             go-github-com-kballard-go-shellquote
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v3")
    (description "Package ccgo translates C to Go source code.")
    (license license:bsd-3)))

(define-public go-modernc-org-lex
  (package
    (name "go-modernc-org-lex")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fad093cdkgdwk3sf0vklk05qzkis1ivri3hig1wigv4z908nmdj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lex"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-fileutil))
    (home-page "https://modernc.org/lex")
    (synopsis #f)
    (description
     "Package lex provides support for a *nix (f)lex like tool on .l sources.  The
syntax is similar to a subset of (f)lex, see also:
@@url{http://flex.sourceforge.net/manual/Format.html#Format,http://flex.sourceforge.net/manual/Format.html#Format}.")
    (license license:bsd-3)))

(define-public go-modernc-org-scannertest
  (package
    (name "go-modernc-org-scannertest")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/scannertest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hk8pqaihhmfxfprg1fmdl2y8ffvrblm10z7qq3l921jjxc1ch7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/scannertest"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-lex))
    (home-page "https://modernc.org/scannertest")
    (synopsis "Package scannertest provides helpers for automated testing of scanners/lexers/tokenizers.")
    (description "Package scannertest provides helpers for automated testing of scanners/lexers/tokenizers.")
    (license license:expat)))

(define-public go-modernc-org-gc
  (package
    (name "go-modernc-org-gc")
    (version "2.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/gc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06bxad6md34ngxfgbwqxwdfw2cgkr2i4s50zwy9afqymds1nlmvh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/gc/v2"
      #:unpack-path "modernc.org/gc"))
    (propagated-inputs (list go-modernc-org-token go-modernc-org-scannertest
                             go-github-com-pmezard-go-difflib
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/gc")
    (synopsis "gc")
    (description
     "Package GC is a Go compiler front end. (Work in progress, API unstable).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "4.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kpjkvk29j56544gczszzvqpirjfcm5i07mah8mbfygf3icd6js1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/ccgo/v4"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-strutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-gc-v2
                             go-modernc-org-fileutil
                             go-modernc-org-ccorpus2
                             go-modernc-org-ccgo-v3
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-mod
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v4")
    (description "Command ccgo is a C compiler producing Go code.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.65.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/libc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l44pb51239c8r7p58s6b96kzcaazqfnfkgcds10d9b7ji08hndh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/libc"))
    (propagated-inputs (list go-modernc-org-memory
                             go-modernc-org-mathutil
                             go-modernc-org-fileutil
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-github-com-ncruces-go-strftime
                             go-github-com-mattn-go-isatty
                             go-github-com-google-uuid
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
     "Package libc is a partial reimplementation of C libc in pure Go.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fzsjkj2zslpccqr9a8pxcz9i3gqvilbqvhci19kmnwy3vd77x0p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/sqlite"))
    (propagated-inputs (list go-modernc-org-mathutil go-modernc-org-libc
                             go-modernc-org-fileutil go-golang-org-x-sys
                             go-github-com-google-pprof))
    (home-page "https://modernc.org/sqlite")
    (synopsis #f)
    (description
     "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-gitea-com-xorm-sqlfiddle
  (package
    (name "go-gitea-com-xorm-sqlfiddle")
    (version "0.0.0-20180821085327-62ce714f951a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/xorm/sqlfiddle.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03g79q02djhk7rjp4k6jh9aaq6z3sqwjcyz4zzml1klv5gwyb1ij"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitea.com/xorm/sqlfiddle"))
    (home-page "https://gitea.com/xorm/sqlfiddle")
    (synopsis "SQL Fiddle API (UnOfficial)")
    (description "This Go library is aimed to provide an API to operate
@@url{http://sqlfiddle.com/,http://sqlfiddle.com/}.")
    (license license:expat)))

(define-public go-xorm-io-builder
  (package
    (name "go-xorm-io-builder")
    (version "0.3.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/xorm/builder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1khn5cmrn3my2sk8pldri2i6ymfy8q7bpc65h3pbi6l4grrmkily"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "xorm.io/builder"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-gitea-com-xorm-sqlfiddle))
    (home-page "https://xorm.io/builder")
    (synopsis "SQL builder")
    (description
     "Package builder is a simple and powerful sql builder for Go.")
    (license license:bsd-3)))

(define-public go-xorm-io-xorm
  (package
    (name "go-xorm-io-xorm")
    (version "1.3.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.com/xorm/xorm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khwhfkxdqq7fyaixqwsx1dv41i6qn148b19s31bcjzfimcsfb38"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "xorm.io/xorm"))
    (propagated-inputs (list go-xorm-io-builder
                             go-modernc-org-sqlite
                             go-github-com-ziutek-mymysql
                             go-github-com-syndtr-goleveldb
                             go-github-com-stretchr-testify
                             go-github-com-shopspring-decimal
                             go-github-com-microsoft-go-mssqldb
                             go-github-com-mattn-go-sqlite3
                             go-github-com-lib-pq
                             go-github-com-json-iterator-go
                             go-github-com-jackc-pgx-v4
                             go-github-com-goccy-go-json
                             go-github-com-go-sql-driver-mysql
                             go-gitee-com-travelliu-dm))
    (home-page "https://xorm.io/xorm")
    (synopsis "xorm")
    (description "Package xorm is a simple and powerful ORM for Go.")
    (license license:bsd-3)))

(define-public go-github-com-6543-go-version
  (package
    (name "go-github-com-6543-go-version")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/6543/go-version")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qlgwxhw9r2r88ap1m9q1hknn4g3xvcdpjgq14gswcqzd34pyg2v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/6543/go-version"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/6543/go-version")
    (synopsis "Versioning Library for Go")
    (description
     "Package version provides functionality for parsing and comparing version
strings.  It supports semantic versioning and includes methods for version
comparison, manipulation, and formatting.")
    (license license:mpl2.0)))

(define-public go-github-com-lunny-vfsgen
  (package
    (name "go-github-com-lunny-vfsgen")
    (version "0.0.0-20200824052919-0d455de96546")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lunny/vfsgen")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0md1vgaq95x1jmxpnsfv6s9xf3v8gqi7lcl7mkxpf6274rf1n2pk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lunny/vfsgen"))
    (home-page "https://github.com/lunny/vfsgen")
    (synopsis "vfsgen")
    (description
     "Package vfsgen takes an http.@code{FileSystem} (likely at `go generate` time)
and generates Go code that statically implements the provided
http.@code{FileSystem}.")
    (license license:expat)))

(define-public go-github-com-netflix-go-expect
  (package
    (name "go-github-com-netflix-go-expect")
    (version "0.0.0-20220104043353-73e0943537d2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Netflix/go-expect")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkvhnc4ii6ygvcsj54ng0kql26rnny7l3hy1w61g88mxjsww1b9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Netflix/go-expect"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-creack-pty))
    (home-page "https://github.com/Netflix/go-expect")
    (synopsis "go-expect")
    (description
     "Package expect provides an expect-like interface to automate control of
applications.  It is unlike expect in that it does not spawn or manage process
lifecycle.  This package only focuses on expecting output and sending input
through it's psuedoterminal.")
    (license license:asl2.0)))

(define-public go-github-com-hinshun-vt10x
  (package
    (name "go-github-com-hinshun-vt10x")
    (version "0.0.0-20220301184237-5011da428d02")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hinshun/vt10x")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pzdwwbzxrsqjb8xfzmfpkyb1gbcszrrimr70cz75jjk2535r26b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hinshun/vt10x"))
    (home-page "https://github.com/hinshun/vt10x")
    (synopsis "vt10x")
    (description
     "Package terminal is a vt10x terminal emulation backend, influenced largely by
st, rxvt, xterm, and @code{iTerm} as reference.  Use it for terminal muxing, a
terminal emulation frontend, or wherever else you need terminal emulation.")
    (license license:expat)))

(define-public go-github-com-alecaivazis-survey
  (package
    (name "go-github-com-alecaivazis-survey")
    (version "2.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlecAivazis/survey")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l3wqphqvm0qxv33pj9f1r72z5fln99vg735fcigv8k513m2aw9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AlecAivazis/survey/v2"
      #:unpack-path "github.com/AlecAivazis/survey"))
    (propagated-inputs (list go-golang-org-x-text
                             go-golang-org-x-term
                             go-github-com-stretchr-testify
                             go-github-com-mgutz-ansi
                             go-github-com-mattn-go-isatty
                             go-github-com-kballard-go-shellquote
                             go-github-com-hinshun-vt10x
                             go-github-com-creack-pty
                             go-github-com-netflix-go-expect))
    (home-page "https://github.com/AlecAivazis/survey")
    (synopsis "Survey")
    (description
     "This package provides a library for building interactive and accessible prompts
on terminals supporting ANSI escape sequences.")
    (license license:expat)))

(define-public go-github-com-andreaskoch-go-fswatch
  (package
    (name "go-github-com-andreaskoch-go-fswatch")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andreaskoch/go-fswatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0caikz1bbb2g9w8hyk7qvwixsy8dvc2gism10927q2cc1100mlr2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/andreaskoch/go-fswatch"))
    (home-page "https://github.com/andreaskoch/go-fswatch")
    (synopsis "fswatch")
    (description
     "fswatch is a go library for watching file system changes to @@strong{does not}
depend on inotify.")
    (license license:bsd-3)))

(define-public go-github-com-imdario-mergo
  (package
    (name "go-github-com-imdario-mergo")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/darccio/mergo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q61904rd858ac19vsmmhz69b1hvn0y9rjfb9d2gc4abg64dva57"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/imdario/mergo"))
    (home-page "https://github.com/imdario/mergo")
    (synopsis "Mergo")
    (description
     "This package provides a helper to merge structs and maps in Golang.  Useful for
configuration default values, avoiding messy if-statements.")
    (license license:bsd-3)))

(define-public go-github-com-armon-circbuf
  (package
    (name "go-github-com-armon-circbuf")
    (version "0.0.0-20190214190532-5111143e8da2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/armon/circbuf")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nhzs8wza5sxqjh0920jypy9irq6cspd55g8a9vgyjjfrqb5njs0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/armon/circbuf"))
    (home-page "https://github.com/armon/circbuf")
    (synopsis "circbuf")
    (description
     "This repository provides the @@code{circbuf} package.  This provides a
@@code{Buffer} object which is a circular (or ring) buffer.  It has a fixed
size, but can be written to infinitely.  Only the last @@code{size} bytes are
ever retained.  The buffer implements the @@code{io.Writer} interface.")
    (license license:expat)))

(define-public go-github-com-data-accelerator-zdfs
  (package
    (name "go-github-com-data-accelerator-zdfs")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/data-accelerator/zdfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j2xr9li2qciqdi6is82aw3fx1lm673bfddgfg3zhbpx3r95mwsy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/data-accelerator/zdfs"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-pkg-errors
                        go-github-com-distribution-reference
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-v2
                        go-github-com-containerd-accelerated-container-image))
    (home-page "https://github.com/data-accelerator/zdfs")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-containerd-accelerated-container-image
  (package
    (name "go-github-com-containerd-accelerated-container-image")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/accelerated-container-image")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0blk72q8nl967k889m6z75g1j5ywc9dhzjd72zm1zlbxm4hb02kv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/accelerated-container-image"))
    (propagated-inputs (list go-oras-land-oras-go-v2
                             go-google-golang-org-grpc
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-urfave-cli-v2
                             go-github-com-spf13-cobra
                             go-github-com-sirupsen-logrus
                             go-github-com-prometheus-client-golang
                             go-github-com-pkg-errors
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-moby-sys-mountinfo
                             go-github-com-moby-locker
                             go-github-com-jessevdk-go-flags
                             go-github-com-go-sql-driver-mysql
                             go-github-com-docker-go-units
                             go-github-com-data-accelerator-zdfs
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-go-cni
                             go-github-com-containerd-errdefs
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2
                             go-github-com-containerd-containerd-api))
    (home-page "https://github.com/containerd/accelerated-container-image")
    (synopsis "Accelerated Container Image")
    (description
     "Accelerated Container Image is an open-source implementation of paper
@@strong{(a (@@ (href
https://www.usenix.org/conference/atc20/presentation/li-huiba) (rel nofollow))
\"DADI: Block-Level Image Service for Agile and Elastic Application Deployment.
USENIX ATC'20\")}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-fuse-overlayfs-snapshotter
  (package
    (name "go-github-com-containerd-fuse-overlayfs-snapshotter")
    (version "2.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/fuse-overlayfs-snapshotter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "002mrbqmm8jzkfwbmrxlx5f4l843dpizc86zl0bchk9kyiygq8fz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/fuse-overlayfs-snapshotter/v2"
      #:unpack-path "github.com/containerd/fuse-overlayfs-snapshotter"))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-github-com-coreos-go-systemd-v22
                             go-github-com-containerd-plugin
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2
                             go-github-com-containerd-containerd-api))
    (home-page "https://github.com/containerd/fuse-overlayfs-snapshotter")
    (synopsis "snapshotter plugin for")
    (description
     "Unlike @@code{overlayfs}, @@code{fuse-overlayfs} can be used as a non-root user
on almost all recent distros.")
    (license license:asl2.0)))

(define-public go-github-com-karpeleslab-reflink
  (package
    (name "go-github-com-karpeleslab-reflink")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KarpelesLab/reflink")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16x01ff1w4xcdl41iicsrsxpk4ba6xf1g1hlq3qx1f098k4s3nci"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/KarpelesLab/reflink"))
    (home-page "https://github.com/KarpelesLab/reflink")
    (synopsis "reflink")
    (description
     "This package provides a Go library to perform efficient file copies using
reflink operations on compatible filesystems (btrfs or xfs).")
    (license license:expat)))

(define-public go-github-com-aliyun-aliyun-oss-go-sdk
  (package
    (name "go-github-com-aliyun-aliyun-oss-go-sdk")
    (version "3.0.2+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aliyun/aliyun-oss-go-sdk")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s9c3if0is8m52hvb1czh4rb7ck4vk654wbzyq44iq1rqfr6y1ly"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aliyun/aliyun-oss-go-sdk"))
    (home-page "https://github.com/aliyun/aliyun-oss-go-sdk")
    (synopsis "Alibaba Cloud OSS SDK for Go")
    (description
     "More example projects can be found at
src\\github.com\\aliyun\\aliyun-oss-go-sdk\\sample under the installation path of
the OSS Go SDK (the first path of the GOPATH variable).  The directory contains
example projects.  Or you can refer to the example objects in the sample
directory under
@@url{https://github.com/aliyun/aliyun-oss-go-sdk&#39;,https://github.com/aliyun/aliyun-oss-go-sdk'}.")
    (license license:expat)))

(define-public go-github-com-freddierice-go-losetup
  (package
    (name "go-github-com-freddierice-go-losetup")
    (version "0.0.0-20220711213114-2a14873012db")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freddierice/go-losetup")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11jhkg07gsv73bc9ya8b5m62hzvhfkjjpvzsvhaj0z5as4y45cr6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/freddierice/go-losetup"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/freddierice/go-losetup")
    (synopsis "go-losetup")
    (description "This package provides a losetup implementation for go-lang.")
    (license license:expat)))

(define-public go-gotest-tools
  (package
    (name "go-gotest-tools")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gotestyourself/gotest.tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ykgj2rpi3yha9rd23abx2885rm72jarhpgw1hkasmrb9i7j6nqk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gotest.tools"))
    (home-page "https://gotest.tools")
    (synopsis "gotest.tools")
    (description
     "Package gotesttools is a collection of packages to augment `testing` and support
common patterns.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-nydus-snapshotter
  (package
    (name "go-github-com-containerd-nydus-snapshotter")
    (version "0.15.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/nydus-snapshotter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x3ry6is9xxvp7n4cp6bx1bmbs1xki0swvv56r3lsjyr1jcfmb1x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/nydus-snapshotter"))
    (propagated-inputs (list go-k8s-io-utils
                        go-k8s-io-cri-api
                        go-k8s-io-client-go
                        go-k8s-io-apimachinery
                        go-k8s-io-api
                        go-gotest-tools
                        go-gopkg-in-natefinch-lumberjack-v2
                        go-google-golang-org-grpc
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-net
                        go-golang-org-x-exp
                        go-go-etcd-io-bbolt
                        go-github-com-urfave-cli-v2
                        go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-rs-xid
                        go-github-com-prometheus-client-model
                        go-github-com-prometheus-client-golang
                        go-github-com-pkg-errors
                        go-github-com-pelletier-go-toml
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-mohae-deepcopy
                        go-github-com-moby-locker
                        go-github-com-klauspost-compress
                        go-github-com-hashicorp-go-retryablehttp
                        go-github-com-gorilla-mux
                        go-github-com-google-go-containerregistry
                        go-github-com-golang-groupcache
                        go-github-com-freddierice-go-losetup
                        go-github-com-docker-cli
                        go-github-com-distribution-reference
                        go-github-com-containers-ocicrypt
                        go-github-com-containerd-stargz-snapshotter-estargz
                        go-github-com-containerd-stargz-snapshotter
                        go-github-com-containerd-plugin
                        go-github-com-containerd-platforms
                        go-github-com-containerd-nri
                        go-github-com-containerd-log
                        go-github-com-containerd-fifo
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-v2
                        go-github-com-containerd-containerd-api
                        go-github-com-containerd-cgroups-v3
                        go-github-com-aws-aws-sdk-go-v2-service-s3
                        go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
                        go-github-com-aws-aws-sdk-go-v2-credentials
                        go-github-com-aws-aws-sdk-go-v2-config
                        go-github-com-aws-aws-sdk-go-v2
                        go-github-com-aliyun-aliyun-oss-go-sdk
                        go-github-com-karpeleslab-reflink
                        go-github-com-adalogics-go-fuzz-headers
                        go-dario-cat-mergo))
    (home-page "https://github.com/containerd/nydus-snapshotter")
    (synopsis "Nydus Snapshotter")
    (description
     "Nydus-snapshotter is a @@strong{non-core} sub-project of containerd.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-stargz-snapshotter
  (package
    (name "go-github-com-containerd-stargz-snapshotter")
    (version "0.16.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/stargz-snapshotter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05l361ib00zwwa262y0i970df27knm5dnr89briw97gm2cn0wk6d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/stargz-snapshotter"))
    (propagated-inputs (list go-k8s-io-cri-api
                             go-k8s-io-client-go
                             go-k8s-io-apimachinery
                             go-k8s-io-api
                             go-google-golang-org-grpc
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-rs-xid
                             go-github-com-prometheus-client-golang
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-moby-sys-mountinfo
                             go-github-com-klauspost-compress
                             go-github-com-hashicorp-go-retryablehttp
                             go-github-com-hashicorp-go-multierror
                             go-github-com-hanwen-go-fuse-v2
                             go-github-com-golang-groupcache
                             go-github-com-gogo-protobuf
                             go-github-com-docker-go-metrics
                             go-github-com-docker-cli
                             go-github-com-distribution-reference
                             go-github-com-containerd-plugin
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-errdefs
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2
                             go-github-com-containerd-console))
    (home-page "https://github.com/containerd/stargz-snapshotter")
    (synopsis "Stargz Snapshotter")
    (description
     "Read also introductory blog:
@@url{https://medium.com/nttlabs/startup-containers-in-lightning-speed-with-lazy-image-distribution-on-containerd-243d94522361,Startup
Containers in Lightning Speed with Lazy Image Distribution on Containerd}.")
    (license license:asl2.0)))

(define-public go-github-com-veraison-go-cose
  (package
    (name "go-github-com-veraison-go-cose")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/veraison/go-cose")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f1fz2x940ri45hh2ahqvc8hgr53hkbv4rmwp9cf3dr4qxf3as63"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/veraison/go-cose"))
    (propagated-inputs (list go-github-com-fxamacker-cbor-v2))
    (home-page "https://github.com/veraison/go-cose")
    (synopsis "go-cose")
    (description
     "This package provides a golang library for the
@@url{https://datatracker.ietf.org/doc/rfc9052/,COSE specification}.")
    (license license:mpl2.0)))

(define-public go-github-com-microsoft-cosesign1go
  (package
    (name "go-github-com-microsoft-cosesign1go")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/cosesign1go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00rc3xn3nn6zank6v4nbby5gvq52h7hk1zdkk14fam6p1fwy60qh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Microsoft/cosesign1go"))
    (propagated-inputs (list go-github-com-veraison-go-cose
                             go-github-com-urfave-cli
                             go-github-com-sirupsen-logrus
                             go-github-com-microsoft-didx509go))
    (home-page "https://github.com/Microsoft/cosesign1go")
    (synopsis "cosesign1go")
    (description
     "This package provides a Go library to handle COSE Sign1 documents.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-backoff
  (package
    (name "go-github-com-lestrrat-go-backoff")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/backoff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s939szsdv0ggp69rig8dkl74s5dvwzm5cw80h0b3dvkqhikim5d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/backoff/v2"
      #:unpack-path "github.com/lestrrat-go/backoff"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-lestrrat-go-option))
    (home-page "https://github.com/lestrrat-go/backoff")
    (synopsis "backoff")
    (description
     "Package backoff implments backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-blackmagic
  (package
    (name "go-github-com-lestrrat-go-blackmagic")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/blackmagic")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyij1wnsh85vqi70sq0kgwrnx4zrn4yx8nk5lqd630g1akqwr8y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/lestrrat-go/blackmagic"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/blackmagic")
    (synopsis "blackmagic")
    (description "Reflect-based black magic.  YMMV, and use with caution.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-httpcc
  (package
    (name "go-github-com-lestrrat-go-httpcc")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/httpcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wsr6ipl3h7iaq7s7a2mgkbli9z5zpxj9dxqhzqn33akb055i28"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/httpcc"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/httpcc")
    (synopsis "httpcc")
    (description
     "Parses HTTP/1.1 Cache-Control header, and returns a struct that is convenient
for the end-user to do what they will with.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-iter
  (package
    (name "go-github-com-lestrrat-go-iter")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/iter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p5fhw5g3kh7c6hvw2mc1r4ckxb3ax262x8b736yyhpv2ynl8jyz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/iter"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/iter")
    (synopsis "iter")
    (description "Simple tools for container iteration.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-option
  (package
    (name "go-github-com-lestrrat-go-option")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/option")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p9744hpdxsnimha5i0gyn7hxn2fy3dxqhlpqvj5s3pc5xv3s14h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/option"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/option")
    (synopsis "option")
    (description "Base object for the \"Optional Parameters Pattern\".")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-jwx
  (package
    (name "go-github-com-lestrrat-go-jwx")
    (version "1.2.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lestrrat-go/jwx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ljbnw7fd9d5xggixrkx7fg9gs4jk23m6xkfy7s1rc7ljkh1n1qk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/lestrrat-go/jwx"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-pkg-errors
                             go-github-com-lestrrat-go-option
                             go-github-com-lestrrat-go-iter
                             go-github-com-lestrrat-go-httpcc
                             go-github-com-lestrrat-go-blackmagic
                             go-github-com-lestrrat-go-backoff-v2
                             go-github-com-goccy-go-json
                             go-github-com-decred-dcrd-dcrec-secp256k1-v4))
    (home-page "https://github.com/lestrrat-go/jwx")
    (synopsis "github.com/lestrrat-go/jwx")
    (description
     "Package jwx contains tools that deal with the various JWx (JOSE) technologies
such as JWT, JWS, JWE, etc in Go.")
    (license license:expat)))

(define-public go-github-com-microsoft-didx509go
  (package
    (name "go-github-com-microsoft-didx509go")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/didx509go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s7gzlfqkyjjyhxpj95448v395fcy6x5kffj2fha8vx7g094fb0k"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Microsoft/didx509go"))
    (propagated-inputs (list go-github-com-lestrrat-go-jwx))
    (home-page "https://github.com/Microsoft/didx509go")
    (synopsis "didx509go")
    (description "DID:x509 resolver for Go.")
    (license license:expat)))

(define-public go-github-com-adamkorcz-go-118-fuzz-build
  (package
    (name "go-github-com-adamkorcz-go-118-fuzz-build")
    (version "0.0.0-20250520111509-a70c2aa677fa")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AdamKorcz/go-118-fuzz-build")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1523sncv9q82abhhmixj27l1frxw1srkkzv7nfsfmkhp9wgdn9b3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AdamKorcz/go-118-fuzz-build"))
    (propagated-inputs (list go-golang-org-x-tools
                             go-github-com-adalogics-go-fuzz-headers))
    (home-page "https://github.com/AdamKorcz/go-118-fuzz-build")
    (synopsis "Go-118-fuzz-build")
    (description
     "Go-118-fuzz-build is a tool to compile native Golang fuzzers to @code{libFuzzer}
fuzzers.  The tool was initially developed because continuous and CI fuzzing
providers have developed platforms that depend on features in fuzzing engines
that the native Go engine was not released with.  To accomodate this,
Go-118-fuzz-build changes the fuzz harnesses into @code{libFuzzer} harnesses
that can then be intrumented with @code{libFuzzer}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-aufs
  (package
    (name "go-github-com-containerd-aufs")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/aufs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jyyyf6sr910m602axmp4h4j1l2n680cpp60z09pvprz55zi4ba0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/aufs"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-pkg-errors
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd))
    (home-page "https://github.com/containerd/aufs")
    (synopsis "aufs snapshotter")
    (description
     "AUFS implementation of the snapshot interface for containerd.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-imgcrypt
  (package
    (name "go-github-com-containerd-imgcrypt")
    (version "1.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/imgcrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vr82v8g45zjm2khkq66fffs4ykkwnc0cjv4ylc5xbgb3z4zm4y7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/imgcrypt"))
    (propagated-inputs (list go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-gogo-protobuf
                             go-github-com-containers-ocicrypt
                             go-github-com-containerd-typeurl
                             go-github-com-containerd-containerd))
    (home-page "https://github.com/containerd/imgcrypt")
    (synopsis "imgcrypt image encryption library and command line tool")
    (description
     "Project @@code{imgcrypt} is a non-core subproject of containerd.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-zfs
  (package
    (name "go-github-com-containerd-zfs")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/zfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1936pkjm71c4x40ww7p8pnwcdcg06j34m1v5zlxywwsl434avscm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/zfs"))
    (propagated-inputs (list go-github-com-mistifyio-go-zfs-v3
                             go-github-com-containerd-log
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd))
    (home-page "https://github.com/containerd/zfs")
    (synopsis "ZFS snapshotter plugin")
    (description "ZFS snapshotter plugin for containerd.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accessapproval
  (package
    (name "go-cloud-google-com-go-accessapproval")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accessapproval"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/accessapproval"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Approval API")
    (description "Go Client Library for Access Approval API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-aiplatform
  (package
    (name "go-cloud-google-com-go-aiplatform")
    (version "1.90.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "aiplatform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/aiplatform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Vertex AI API")
    (description "Go Client Library for Vertex AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-analytics
  (package
    (name "go-cloud-google-com-go-analytics")
    (version "0.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "analytics"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/analytics"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Analytics API")
    (description "Go Client Library for Analytics API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigateway
  (package
    (name "go-cloud-google-com-go-apigateway")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigateway"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigateway"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "API Gateway API")
    (description "Go Client Library for API Gateway API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeconnect
  (package
    (name "go-cloud-google-com-go-apigeeconnect")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigeeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Connect API")
    (description "Go Client Library for Apigee Connect API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-apigeeregistry
  (package
    (name "go-cloud-google-com-go-apigeeregistry")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "apigeeregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/apigeeregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Apigee Registry API")
    (description "Go Client Library for Apigee Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-appengine
  (package
    (name "go-cloud-google-com-go-appengine")
    (version "1.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "appengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/appengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "App Engine Admin API")
    (description "Go Client Library for App Engine Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-area120
  (package
    (name "go-cloud-google-com-go-area120")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "area120"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/area120"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Area120 API")
    (description "Go Client Library for Area120 API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-artifactregistry
  (package
    (name "go-cloud-google-com-go-artifactregistry")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "artifactregistry"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/artifactregistry"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Artifact Registry API")
    (description "Go Client Library for Artifact Registry API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-accesscontextmanager
  (package
    (name "go-cloud-google-com-go-accesscontextmanager")
    (version "1.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "accesscontextmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/accesscontextmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Access Context Manager API")
    (description "Go Client Library for Access Context Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-asset
  (package
    (name "go-cloud-google-com-go-asset")
    (version "1.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "asset"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/asset"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-accesscontextmanager))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Asset API")
    (description "Go Client Library for Cloud Asset API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-assuredworkloads
  (package
    (name "go-cloud-google-com-go-assuredworkloads")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "assuredworkloads"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/assuredworkloads"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Assured Workloads API")
    (description "Go Client Library for Assured Workloads API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-automl
  (package
    (name "go-cloud-google-com-go-automl")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "automl"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/automl"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud AutoML API")
    (description "Go Client Library for Cloud @code{AutoML} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-baremetalsolution
  (package
    (name "go-cloud-google-com-go-baremetalsolution")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "baremetalsolution"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/baremetalsolution"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Bare Metal Solution API")
    (description "Go Client Library for Bare Metal Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-batch
  (package
    (name "go-cloud-google-com-go-batch")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "batch"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/batch"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Batch API")
    (description "Go Client Library for Batch API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-beyondcorp
  (package
    (name "go-cloud-google-com-go-beyondcorp")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "beyondcorp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/beyondcorp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "BeyondCorp API")
    (description "Go Client Library for @code{BeyondCorp} API.")
    (license license:asl2.0)))

(define-public go-github-com-apache-thrift
  (package
    (name "go-github-com-apache-thrift")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/thrift")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vkmm7g87lmgsbflgnmrvjrj8lvk87s3mfn93hl13zh07pw0wq40"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/apache/thrift"))
    (home-page "https://github.com/apache/thrift")
    (synopsis "Apache Thrift")
    (description
     "Thrift is a lightweight, language-independent software stack for point-to-point
RPC implementation.  Thrift provides clean abstractions and implementations for
data transport, data serialization, and application level processing.  The code
generation system takes a simple definition language as input and generates code
across programming languages that uses the abstracted stack to build
interoperable RPC clients and servers.")
    (license license:asl2.0)))

(define-public go-github-com-minio-asm2plan9s
  (package
    (name "go-github-com-minio-asm2plan9s")
    (version "0.0.0-20200509001527-cdd76441f9d8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/asm2plan9s")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i635ipzfqy7cyj68sl3mmqbnjqgyrhjxpyp62z2dbm34i42pfbg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/asm2plan9s"))
    (home-page "https://github.com/minio/asm2plan9s")
    (synopsis "asm2plan9s")
    (description
     "Tool to generate BYTE sequences for Go assembly as generated by YASM/GAS (for
Intel) or GAS (for ARM).")
    (license license:asl2.0)))

(define-public go-github-com-minio-c2goasm
  (package
    (name "go-github-com-minio-c2goasm")
    (version "0.0.0-20190812172519-36a3d3bbc4f3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/c2goasm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aw8g7455r7av7s4sdc57yyd2d5298linppx8m4cfhrgmd6rblzf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/c2goasm"))
    (home-page "https://github.com/minio/c2goasm")
    (synopsis "c2goasm: C to Go Assembly")
    (description
     "This is a tool to convert assembly as generated by a C/C++ compiler into Golang
assembly.  It is meant to be used in combination with
@@url{https://github.com/minio/asm2plan9s,asm2plan9s} in order to automatically
generate pure Go wrappers for C/C++ code (that may for instance take advantage
of compiler SIMD intrinsics or @@code{template<>} code).")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-xxh3
  (package
    (name "go-github-com-zeebo-xxh3")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/xxh3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gy666r5v1d1n2cfig9plhyp7z09f06k6mr5lrf0mk6psk6bnwgi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/xxh3"))
    (propagated-inputs (list go-github-com-zeebo-assert
                             go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/zeebo/xxh3")
    (synopsis "XXH3")
    (description
     "This package is a port of the
@@url{https://github.com/Cyan4973/@code{xxHash,xxh3}} library to Go.")
    (license license:bsd-2)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "4.26.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j3j9w3p66wgi0ckiw8rvaahqxcp5ai1ndikxpcx7qb9mv2m3qp6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v4"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-sortutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-ccorpus2
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v4")
    (description "Package cc is a C99 compiler front end.")
    (license license:bsd-3)))

(define-public go-modernc-org-cc
  (package
    (name "go-modernc-org-cc")
    (version "3.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khw9qsaz4ab0vb4kazgfm481cjpcyxj6ld2ma4d9hva3ca9h8ji"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/cc/v3"
      #:unpack-path "modernc.org/cc"))
    (propagated-inputs (list go-modernc-org-token
                             go-modernc-org-strutil
                             go-modernc-org-mathutil
                             go-lukechampine-com-uint128
                             go-github-com-google-go-cmp
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccorpus
  (package
    (name "go-modernc-org-ccorpus")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccorpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18d5npw8aw5qzy6qcrlrili2zxvmc2v4kkwjps6c3ayvi7aj7j09"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccorpus"))
    (propagated-inputs (list go-modernc-org-httpfs))
    (home-page "https://modernc.org/ccorpus")
    (synopsis "ccorpus")
    (description "Package ccorpus provides a test corpus of C code.")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "3.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zkxzhmkm5pgalcnlhw0whiii6vhdpnnnpwkx9b5ah21ajk6qqlc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/ccgo/v3"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-ccorpus
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v3
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-github-com-pmezard-go-difflib
                             go-github-com-kballard-go-shellquote
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v3")
    (description "Package ccgo translates C to Go source code.")
    (license license:bsd-3)))

(define-public go-modernc-org-lex
  (package
    (name "go-modernc-org-lex")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/lex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fad093cdkgdwk3sf0vklk05qzkis1ivri3hig1wigv4z908nmdj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/lex"))
    (propagated-inputs (list go-modernc-org-lexer go-modernc-org-fileutil))
    (home-page "https://modernc.org/lex")
    (synopsis #f)
    (description
     "Package lex provides support for a *nix (f)lex like tool on .l sources.  The
syntax is similar to a subset of (f)lex, see also:
@@url{http://flex.sourceforge.net/manual/Format.html#Format,http://flex.sourceforge.net/manual/Format.html#Format}.")
    (license license:bsd-3)))

(define-public go-modernc-org-gc
  (package
    (name "go-modernc-org-gc")
    (version "2.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/gc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06bxad6md34ngxfgbwqxwdfw2cgkr2i4s50zwy9afqymds1nlmvh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "modernc.org/gc/v2"
      #:unpack-path "modernc.org/gc"))
    (propagated-inputs (list go-modernc-org-token go-modernc-org-scannertest
                             go-github-com-pmezard-go-difflib
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/gc")
    (synopsis "gc")
    (description
     "Package GC is a Go compiler front end. (Work in progress, API unstable).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo
  (package
    (name "go-modernc-org-ccgo")
    (version "4.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/ccgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kpjkvk29j56544gczszzvqpirjfcm5i07mah8mbfygf3icd6js1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/ccgo/v4"
      #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs (list go-modernc-org-strutil
                             go-modernc-org-opt
                             go-modernc-org-mathutil
                             go-modernc-org-libc
                             go-modernc-org-gc-v2
                             go-modernc-org-fileutil
                             go-modernc-org-ccorpus2
                             go-modernc-org-ccgo-v3
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-mod
                             go-github-com-pmezard-go-difflib
                             go-github-com-pbnjay-memory
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo")
    (synopsis "ccgo/v4")
    (description "Command ccgo is a C compiler producing Go code.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.65.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/libc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l44pb51239c8r7p58s6b96kzcaazqfnfkgcds10d9b7ji08hndh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/libc"))
    (propagated-inputs (list go-modernc-org-memory
                             go-modernc-org-mathutil
                             go-modernc-org-fileutil
                             go-modernc-org-ccgo-v4
                             go-modernc-org-cc-v4
                             go-golang-org-x-tools
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-github-com-ncruces-go-strftime
                             go-github-com-mattn-go-isatty
                             go-github-com-google-uuid
                             go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
     "Package libc is a partial reimplementation of C libc in pure Go.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.38.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/cznic/sqlite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fzsjkj2zslpccqr9a8pxcz9i3gqvilbqvhci19kmnwy3vd77x0p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "modernc.org/sqlite"))
    (propagated-inputs (list go-modernc-org-mathutil go-modernc-org-libc
                             go-modernc-org-fileutil go-golang-org-x-sys
                             go-github-com-google-pprof))
    (home-page "https://modernc.org/sqlite")
    (synopsis #f)
    (description
     "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-github-com-hamba-avro
  (package
    (name "go-github-com-hamba-avro")
    (version "2.29.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hamba/avro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hiv4aydi8x1s4vjasfrrnbj328l2g6g2x5smag1crl1zz57v115"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/hamba/avro/v2"
      #:unpack-path "github.com/hamba/avro"))
    (propagated-inputs (list go-golang-org-x-tools
                             go-github-com-stretchr-testify
                             go-github-com-modern-go-reflect2
                             go-github-com-klauspost-compress
                             go-github-com-json-iterator-go
                             go-github-com-golang-snappy
                             go-github-com-go-viper-mapstructure-v2
                             go-github-com-ettle-strcase))
    (home-page "https://github.com/hamba/avro")
    (synopsis "Overview")
    (description
     "Package avro implements encoding and decoding of Avro as defined by the Avro
specification.")
    (license license:expat)))

(define-public go-github-com-substrait-io-substrait
  (package
    (name "go-github-com-substrait-io-substrait")
    (version "0.73.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c25493yvzjij6wszd9xhb2cxzl166ywm9a5i9g2ha8db8bky6gy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/substrait-io/substrait"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/substrait-io/substrait")
    (synopsis "Substrait")
    (description
     "Package substrait provides access to Substrait artifacts via embed.FS. Use
@code{substrait.GetSubstraitFS()} to retrieve the embed.FS object.")
    (license license:asl2.0)))

(define-public go-github-com-substrait-io-substrait-go
  (package
    (name "go-github-com-substrait-io-substrait-go")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/substrait-io/substrait-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "032d2d5dgapv5dypp3h6m0zzm20x79ypljwc02fw5gcpbli3x2m7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/substrait-io/substrait-go"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-protobuf
                             go-golang-org-x-exp
                             go-github-com-substrait-io-substrait
                             go-github-com-stretchr-testify
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-goccy-go-yaml
                             go-github-com-creasty-defaults
                             go-github-com-cockroachdb-apd-v3
                             go-github-com-alecthomas-participle-v2))
    (home-page "https://github.com/substrait-io/substrait-go")
    (synopsis "substrait-go")
    (description
     "Package substraitgo contains the experimental go bindings for substrait
(@@url{https://substrait.io,https://substrait.io}).")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-bigquery
  (package
    (name "go-cloud-google-com-go-bigquery")
    (version "1.69.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigquery"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "021dnqqs86va6gqnaa1p4wmsvphkzs36malbw23r913a26r77iqf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/bigquery"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-xerrors
                             go-golang-org-x-sync
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-apache-arrow-go-v15
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "BigQuery")
    (description
     "Package bigquery provides a client for the @code{BigQuery} service.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-cloud-bigtable-clients-test
  (package
    (name "go-github-com-googleapis-cloud-bigtable-clients-test")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/cloud-bigtable-clients-test")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05nvmwmhj94na0a7nzmjh7703qji5pf9a0sjf219hwcl794hi26q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/cloud-bigtable-clients-test"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-bigtable))
    (home-page "https://github.com/googleapis/cloud-bigtable-clients-test")
    (synopsis "Test Framework for Cloud Bigtable Client Libraries")
    (description
     "This repository contains the test framework to validate the correctness of Cloud
Bigtable @@url{https://cloud.google.com/bigtable/docs/reference/libraries,client
libraries}.  Specifically, all of the client libraries should exhibit correct
and consistent behaviors when interacting with the server (e.g. retry on
transient error) However, writing test cases in every language would present
maintainability and scalability challenges.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-bigtable
  (package
    (name "go-cloud-google-com-go-bigtable")
    (version "1.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "bigtable"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0al2fpb32izmm2ydhwx752rxnag594xx5i88ajxi9r31sy05vckx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/bigtable"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-rsc-io-binaryregexp
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-cloud-bigtable-clients-test
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-google-btree
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package bigtable is an API to Google Cloud Bigtable.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-billing
  (package
    (name "go-cloud-google-com-go-billing")
    (version "1.20.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "billing"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/billing"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Billing API")
    (description "Go Client Library for Cloud Billing API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-binaryauthorization
  (package
    (name "go-cloud-google-com-go-binaryauthorization")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "binaryauthorization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/binaryauthorization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Binary Authorization API")
    (description "Go Client Library for Binary Authorization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-certificatemanager
  (package
    (name "go-cloud-google-com-go-certificatemanager")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "certificatemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/certificatemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Certificate Manager API")
    (description "Go Client Library for Certificate Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-channel
  (package
    (name "go-cloud-google-com-go-channel")
    (version "1.19.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "channel"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/channel"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Channel API")
    (description "Go Client Library for Cloud Channel API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudbuild
  (package
    (name "go-cloud-google-com-go-cloudbuild")
    (version "1.22.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudbuild"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/cloudbuild"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Build API")
    (description "Go Client Library for Cloud Build API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-clouddms
  (package
    (name "go-cloud-google-com-go-clouddms")
    (version "1.8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "clouddms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/clouddms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Database Migration API")
    (description "Go Client Library for Database Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-cloudtasks
  (package
    (name "go-cloud-google-com-go-cloudtasks")
    (version "1.13.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "cloudtasks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/cloudtasks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Tasks API")
    (description "Go Client Library for Cloud Tasks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-compute
  (package
    (name "go-cloud-google-com-go-compute")
    (version "1.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "compute"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/compute"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Compute API")
    (description "Go Client Library for Compute API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-contactcenterinsights
  (package
    (name "go-cloud-google-com-go-contactcenterinsights")
    (version "1.17.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "contactcenterinsights"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/contactcenterinsights"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Contact Center AI Insights API")
    (description "Go Client Library for Contact Center AI Insights API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-container
  (package
    (name "go-cloud-google-com-go-container")
    (version "1.43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "container"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/container"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Kubernetes Engine API")
    (description
     "Package container contains a deprecated Google Container Engine client.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-grafeas
  (package
    (name "go-cloud-google-com-go-grafeas")
    (version "0.3.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "grafeas"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/grafeas"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Grafeas API")
    (description "Go Client Library for Grafeas API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-containeranalysis
  (package
    (name "go-cloud-google-com-go-containeranalysis")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "containeranalysis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/containeranalysis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-grafeas
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Container Analysis API")
    (description "Go Client Library for Container Analysis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datacatalog
  (package
    (name "go-cloud-google-com-go-datacatalog")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datacatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10xfkc3igsw95fs13k8w0qjig6fsfpd1j9kar43jx74wqlhs02rb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datacatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Data Catalog API")
    (description "Go Client Library for Google Cloud Data Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataflow
  (package
    (name "go-cloud-google-com-go-dataflow")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataflow API")
    (description "Go Client Library for Dataflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataform
  (package
    (name "go-cloud-google-com-go-dataform")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataform"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataform"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataform API")
    (description "Go Client Library for Dataform API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datafusion
  (package
    (name "go-cloud-google-com-go-datafusion")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datafusion"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datafusion"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Fusion API")
    (description "Go Client Library for Cloud Data Fusion API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datalabeling
  (package
    (name "go-cloud-google-com-go-datalabeling")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datalabeling"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datalabeling"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data Labeling API")
    (description "Go Client Library for Data Labeling API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataplex
  (package
    (name "go-cloud-google-com-go-dataplex")
    (version "1.25.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataplex"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataplex"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataplex API")
    (description "Go Client Library for Cloud Dataplex API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataproc
  (package
    (name "go-cloud-google-com-go-dataproc")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataproc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataproc/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Dataproc API")
    (description "Go Client Library for Cloud Dataproc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dataqna
  (package
    (name "go-cloud-google-com-go-dataqna")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dataqna"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dataqna"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Data QnA API")
    (description "Go Client Library for Data @code{QnA} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastore
  (package
    (name "go-cloud-google-com-go-datastore")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fj48fav12jrg3dzbbal8h5rv3xhgq0kc9vnihnxdj5nvbig9y8f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/datastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Datastore")
    (description
     "Package datastore provides a client for Google Cloud Datastore.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-datastream
  (package
    (name "go-cloud-google-com-go-datastream")
    (version "1.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "datastream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/datastream"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Datastream API")
    (description "Go Client Library for Datastream API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-deploy
  (package
    (name "go-cloud-google-com-go-deploy")
    (version "1.27.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "deploy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ryq4ay3myk7w2wb7pzfk0pbvz6ymirxq91zm6rql7a1vb15x0n9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/deploy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Deploy API")
    (description "Go Client Library for Google Cloud Deploy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dialogflow
  (package
    (name "go-cloud-google-com-go-dialogflow")
    (version "1.68.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dialogflow"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dialogflow"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dialogflow API")
    (description "Go Client Library for Dialogflow API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-dlp
  (package
    (name "go-cloud-google-com-go-dlp")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "dlp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/dlp"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Data Loss Prevention (DLP) API")
    (description "Go Client Library for Cloud Data Loss Prevention (DLP) API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-documentai
  (package
    (name "go-cloud-google-com-go-documentai")
    (version "1.37.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "documentai"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10xfkc3igsw95fs13k8w0qjig6fsfpd1j9kar43jx74wqlhs02rb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/documentai"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Document AI API")
    (description "Go Client Library for Cloud Document AI API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-domains
  (package
    (name "go-cloud-google-com-go-domains")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "domains"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/domains"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Domains API")
    (description "Go Client Library for Cloud Domains API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-edgecontainer
  (package
    (name "go-cloud-google-com-go-edgecontainer")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "edgecontainer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/edgecontainer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Distributed Cloud Edge Container API")
    (description "Go Client Library for Distributed Cloud Edge Container API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-errorreporting
  (package
    (name "go-cloud-google-com-go-errorreporting")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "errorreporting"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00bdhr81cr37vy0llh0sifhx0ya5izhdwy95y72ykhavvivlksyd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/errorreporting"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Error Reporting API")
    (description
     "Package errorreporting is a Google Cloud Error Reporting library.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-essentialcontacts
  (package
    (name "go-cloud-google-com-go-essentialcontacts")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "essentialcontacts"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/essentialcontacts"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Essential Contacts API")
    (description "Go Client Library for Essential Contacts API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-eventarc
  (package
    (name "go-cloud-google-com-go-eventarc")
    (version "1.15.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "eventarc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/eventarc"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Eventarc API")
    (description "Go Client Library for Eventarc API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-filestore
  (package
    (name "go-cloud-google-com-go-filestore")
    (version "1.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "filestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/filestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Filestore API")
    (description "Go Client Library for Cloud Filestore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-firestore
  (package
    (name "go-cloud-google-com-go-firestore")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "firestore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cm1yn7d26cqar9nf1yw9ml3kqdqbhy2rv9m441sqgwbm44b10da"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/firestore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description
     "Package firestore provides a client for reading and writing to a Cloud Firestore
database.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-functions
  (package
    (name "go-cloud-google-com-go-functions")
    (version "1.19.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "functions"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/functions"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Functions API")
    (description "Go Client Library for Cloud Functions API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkebackup
  (package
    (name "go-cloud-google-com-go-gkebackup")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkebackup"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkebackup"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Backup for GKE API")
    (description "Go Client Library for Backup for GKE API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkeconnect
  (package
    (name "go-cloud-google-com-go-gkeconnect")
    (version "0.12.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkeconnect"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkeconnect"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Connect APIs")
    (description "Go Client Library for GKE Connect APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkehub
  (package
    (name "go-cloud-google-com-go-gkehub")
    (version "0.15.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkehub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkehub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "GKE Hub")
    (description "Go Client Library for GKE Hub.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gkemulticloud
  (package
    (name "go-cloud-google-com-go-gkemulticloud")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gkemulticloud"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gkemulticloud"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Anthos Multi-Cloud API")
    (description "Go Client Library for Anthos Multi-Cloud API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-gsuiteaddons
  (package
    (name "go-cloud-google-com-go-gsuiteaddons")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "gsuiteaddons"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/gsuiteaddons"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Workspace Add-ons API")
    (description "Go Client Library for Google Workspace Add-ons API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iap
  (package
    (name "go-cloud-google-com-go-iap")
    (version "1.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iap"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iap"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Identity-Aware Proxy API")
    (description "Go Client Library for Cloud Identity-Aware Proxy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-ids
  (package
    (name "go-cloud-google-com-go-ids")
    (version "1.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "ids"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/ids"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IDS API")
    (description "Go Client Library for Cloud IDS API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iot
  (package
    (name "go-cloud-google-com-go-iot")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iot"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iot"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud IoT API")
    (description "Go Client Library for Cloud @code{IoT} API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-language
  (package
    (name "go-cloud-google-com-go-language")
    (version "1.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "language"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/language"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Natural Language API")
    (description "Go Client Library for Cloud Natural Language API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-lifesciences
  (package
    (name "go-cloud-google-com-go-lifesciences")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "lifesciences"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/lifesciences"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Life Sciences API")
    (description "Go Client Library for Cloud Life Sciences API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-logging
  (package
    (name "go-cloud-google-com-go-logging")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "logging"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18v0lcwn97i8amqd054i55xnx9gclcj9gwyrrqdwjrh5g8kmc19c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/logging"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opencensus-io
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-compute-metadata
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Logging")
    (description
     "Package logging contains a Cloud Logging client suitable for writing logs.  For
reading logs, and working with sinks, metrics and monitored resources, see
package cloud.google.com/go/logging/logadmin.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-managedidentities
  (package
    (name "go-cloud-google-com-go-managedidentities")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "managedidentities"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/managedidentities"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Managed Service for Microsoft Active Directory API")
    (description
     "Go Client Library for Managed Service for Microsoft Active Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-maps
  (package
    (name "go-cloud-google-com-go-maps")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "maps"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/maps"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Maps Platform APIs")
    (description "Go Client Library for Google Maps Platform APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-mediatranslation
  (package
    (name "go-cloud-google-com-go-mediatranslation")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "mediatranslation"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/mediatranslation"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Media Translation API")
    (description "Go Client Library for Media Translation API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-memcache
  (package
    (name "go-cloud-google-com-go-memcache")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "memcache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/memcache"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Memorystore for Memcached API")
    (description "Go Client Library for Cloud Memorystore for Memcached API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-metastore
  (package
    (name "go-cloud-google-com-go-metastore")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "metastore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/metastore"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Dataproc Metastore API")
    (description "Go Client Library for Dataproc Metastore API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkconnectivity
  (package
    (name "go-cloud-google-com-go-networkconnectivity")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkconnectivity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networkconnectivity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Connectivity API")
    (description "Go Client Library for Network Connectivity API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networkmanagement
  (package
    (name "go-cloud-google-com-go-networkmanagement")
    (version "1.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networkmanagement"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networkmanagement"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Management API")
    (description "Go Client Library for Network Management API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-networksecurity
  (package
    (name "go-cloud-google-com-go-networksecurity")
    (version "0.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "networksecurity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/networksecurity"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Network Security API")
    (description "Go Client Library for Network Security API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-notebooks
  (package
    (name "go-cloud-google-com-go-notebooks")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "notebooks"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/notebooks"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Notebooks API")
    (description "Go Client Library for Notebooks API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-optimization
  (package
    (name "go-cloud-google-com-go-optimization")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "optimization"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/optimization"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Optimization API")
    (description "Go Client Library for Cloud Optimization API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orchestration
  (package
    (name "go-cloud-google-com-go-orchestration")
    (version "1.11.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orchestration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zvp47kv8wyhwhkgqy89h5kzf94rdk3dbsqm0hggl0b5mq6pc1iw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/orchestration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Composer API")
    (description "Go Client Library for Cloud Composer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-orgpolicy
  (package
    (name "go-cloud-google-com-go-orgpolicy")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "orgpolicy"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n1bmmsbyfljw4x4p0gfcgpqw17wca6r4dkz98xn41mbhpi6dhkk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/orgpolicy"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Organization Policy API")
    (description "Go Client Library for Organization Policy API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-osconfig
  (package
    (name "go-cloud-google-com-go-osconfig")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "osconfig"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/osconfig"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "OS Config API")
    (description "Go Client Library for OS Config API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-oslogin
  (package
    (name "go-cloud-google-com-go-oslogin")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "oslogin"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/oslogin"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud OS Login API")
    (description "Go Client Library for Cloud OS Login API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-phishingprotection
  (package
    (name "go-cloud-google-com-go-phishingprotection")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "phishingprotection"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/phishingprotection"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Phishing Protection API")
    (description "Go Client Library for Phishing Protection API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-policytroubleshooter
  (package
    (name "go-cloud-google-com-go-policytroubleshooter")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "policytroubleshooter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/policytroubleshooter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Policy Troubleshooter API")
    (description "Go Client Library for Policy Troubleshooter API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-privatecatalog
  (package
    (name "go-cloud-google-com-go-privatecatalog")
    (version "0.10.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "privatecatalog"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/privatecatalog"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Private Catalog API")
    (description "Go Client Library for Cloud Private Catalog API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-kms
  (package
    (name "go-cloud-google-com-go-kms")
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "kms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/kms"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Key Management Service (KMS) API")
    (description
     "Go Client Library for Cloud Key Management Service (KMS) API.")
    (license license:asl2.0)))

(define-public go-go-einride-tech-aip
  (package
    (name "go-go-einride-tech-aip")
    (version "0.70.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/einride/aip-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09g8hhlclqccwixn39fjd1n5ip719dwccydxdjsi8xfjy8lbyrr7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.einride.tech/aip"))
    (propagated-inputs (list go-gotest-tools-v3
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-github-com-stoewer-go-strcase
                             go-github-com-google-uuid))
    (home-page "https://go.einride.tech/aip")
    (synopsis "AIP Go")
    (description
     "Package aip provides primitives for implementing API Improvement Proposals
(AIP).")
    (license license:expat)))

(define-public go-cloud-google-com-go-pubsub
  (package
    (name "go-cloud-google-com-go-pubsub")
    (version "1.49.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsub"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01bllmiirmmkw040i1pxfc71zsv8lm3cbziqv1vk5b4ryys6smky"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/pubsub"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-time
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-go-opencensus-io
                             go-go-einride-tech-aip
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Pub/Sub")
    (description
     "Package pubsub provides an easy way to publish and receive Google Cloud Pub/Sub
messages, hiding the details of the underlying server RPCs.  Pub/Sub is a
many-to-many, asynchronous messaging system that decouples senders and
receivers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-pubsublite
  (package
    (name "go-cloud-google-com-go-pubsublite")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "pubsublite"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7ycyzxbk6k4s63r0f8crb8i4jcc3lsk5n2wcnfdk6qkzs15572"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/pubsublite"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-oauth2
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Pub/Sub Lite")
    (description
     "Package pubsublite provides an easy way to publish and receive messages using
the Pub/Sub Lite service.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recaptchaenterprise
  (package
    (name "go-cloud-google-com-go-recaptchaenterprise")
    (version "2.20.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recaptchaenterprise"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zvp47kv8wyhwhkgqy89h5kzf94rdk3dbsqm0hggl0b5mq6pc1iw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recaptchaenterprise/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "reCAPTCHA Enterprise API")
    (description "Go Client Library for @code{reCAPTCHA} Enterprise API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommendationengine
  (package
    (name "go-cloud-google-com-go-recommendationengine")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommendationengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recommendationengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommendations AI")
    (description "Go Client Library for Recommendations AI.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-recommender
  (package
    (name "go-cloud-google-com-go-recommender")
    (version "1.13.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "recommender"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/recommender"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Recommender API")
    (description "Go Client Library for Recommender API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-redis
  (package
    (name "go-cloud-google-com-go-redis")
    (version "1.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "redis"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/redis"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Memorystore for Redis API")
    (description
     "Go Client Library for Google Cloud Memorystore for Redis API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcemanager
  (package
    (name "go-cloud-google-com-go-resourcemanager")
    (version "1.10.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcemanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/resourcemanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Resource Manager API")
    (description "Go Client Library for Cloud Resource Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-resourcesettings
  (package
    (name "go-cloud-google-com-go-resourcesettings")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "resourcesettings"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w04dgxk0lx5k4s255ladf5w8n2nivvhp5vlyb2va96j5aa8j2q5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "cloud.google.com/go/resourcesettings"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Resource Settings API")
    (description "Go Client Library for Resource Settings API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-retail
  (package
    (name "go-cloud-google-com-go-retail")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "retail"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/retail"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Retail API")
    (description "Go Client Library for Retail API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-run
  (package
    (name "go-cloud-google-com-go-run")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "run"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hydiwi5ilwcf52rkd627w7834cwai9zw9yqik4m3a6v263fx4dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/run"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Run Admin API")
    (description "Go Client Library for Cloud Run Admin API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-scheduler
  (package
    (name "go-cloud-google-com-go-scheduler")
    (version "1.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "scheduler"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/scheduler"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Scheduler API")
    (description "Go Client Library for Cloud Scheduler API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-secretmanager
  (package
    (name "go-cloud-google-com-go-secretmanager")
    (version "1.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "secretmanager"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/secretmanager"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Secret Manager API")
    (description "Go Client Library for Secret Manager API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-security
  (package
    (name "go-cloud-google-com-go-security")
    (version "1.18.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "security"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/security"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security APIs")
    (description "Go Client Library for Security APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-securitycenter
  (package
    (name "go-cloud-google-com-go-securitycenter")
    (version "1.36.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "securitycenter"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/securitycenter"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Security Command Center API")
    (description "Go Client Library for Security Command Center API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-servicedirectory
  (package
    (name "go-cloud-google-com-go-servicedirectory")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "servicedirectory"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/servicedirectory"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "Service Directory API")
    (description "Go Client Library for Service Directory API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-shell
  (package
    (name "go-cloud-google-com-go-shell")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "shell"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/shell"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Shell API")
    (description "Go Client Library for Cloud Shell API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
  (package
    (name "go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp")
    (version "1.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
             (commit (go-version->git-ref version
                                          #:subdir "grpcgcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rhcs9fj78z6fsfbyzra1l0n4bwmn74rg4shqhx91jhg2hkhazd7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/GoogleCloudPlatform/grpc-gcp-go/grpcgcp"
      #:unpack-path "github.com/GoogleCloudPlatform/grpc-gcp-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-google-go-cmp
                             go-github-com-golang-mock))
    (home-page "https://github.com/GoogleCloudPlatform/grpc-gcp-go")
    (synopsis "How to test Spanner integration")
    (description
     "Package grpcgcp provides grpc supports for Google Cloud APIs.  For now it
provides connection management with affinity support.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-spanner
  (package
    (name "go-cloud-google-com-go-spanner")
    (version "1.82.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "spanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lii7881p7jhvfizdf8sf39yl15nw4ganbmdd6rid4ld2vm87pjj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/spanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-go-opencensus-io
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-grpc-gcp-go-grpcgcp
                        go-cloud-google-com-go-monitoring
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Spanner")
    (description
     "Package spanner provides a client for reading and writing to Cloud Spanner
databases.  See the packages under admin for clients that operate on databases
and instances.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-speech
  (package
    (name "go-cloud-google-com-go-speech")
    (version "1.27.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "speech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/speech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Speech-to-Text API")
    (description "Go Client Library for Cloud Speech-to-Text API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-storagetransfer
  (package
    (name "go-cloud-google-com-go-storagetransfer")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storagetransfer"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "155qvn234bxz21p75zqxznz59g074l0diy0s1nxqkxwwfmn2vbpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/storagetransfer"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Storage Transfer API")
    (description "Go Client Library for Storage Transfer API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-talent
  (package
    (name "go-cloud-google-com-go-talent")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "talent"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/talent"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Talent Solution API")
    (description "Go Client Library for Cloud Talent Solution API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-texttospeech
  (package
    (name "go-cloud-google-com-go-texttospeech")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "texttospeech"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pgz76i9vliwsx10xc7k303352jm8h9pdfnjqlc5z9qqyqf8fc9w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/texttospeech"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Text-to-Speech API")
    (description "Go Client Library for Cloud Text-to-Speech API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-tpu
  (package
    (name "go-cloud-google-com-go-tpu")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "tpu"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/tpu"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud TPU API")
    (description "Go Client Library for Cloud TPU API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-trace
  (package
    (name "go-cloud-google-com-go-trace")
    (version "1.11.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "trace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/trace"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Stackdriver Trace API")
    (description "Go Client Library for Stackdriver Trace API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-video
  (package
    (name "go-cloud-google-com-go-video")
    (version "1.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "video"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2sncxbrxg0534b020b51wvkcajawp2f6z0pzy7qqfngkh9v5yb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/video"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Video APIs")
    (description "Go Client Library for Video APIs.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-videointelligence
  (package
    (name "go-cloud-google-com-go-videointelligence")
    (version "1.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "videointelligence"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/videointelligence"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Video Intelligence API")
    (description "Go Client Library for Google Cloud Video Intelligence API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vision
  (package
    (name "go-cloud-google-com-go-vision")
    (version "2.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vision"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vision/v2"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Vision API")
    (description "Go Client Library for Cloud Vision API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmmigration
  (package
    (name "go-cloud-google-com-go-vmmigration")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmmigration"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vmmigration"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VM Migration API")
    (description "Go Client Library for VM Migration API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vmwareengine
  (package
    (name "go-cloud-google-com-go-vmwareengine")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vmwareengine"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vmwareengine"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-iam))
    (home-page "https://cloud.google.com/go")
    (synopsis "VMware Engine API")
    (description "Go Client Library for VMware Engine API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-vpcaccess
  (package
    (name "go-cloud-google-com-go-vpcaccess")
    (version "1.8.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "vpcaccess"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/vpcaccess"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Serverless VPC Access API")
    (description "Go Client Library for Serverless VPC Access API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-webrisk
  (package
    (name "go-cloud-google-com-go-webrisk")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "webrisk"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/webrisk"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Risk API")
    (description "Go Client Library for Web Risk API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-websecurityscanner
  (package
    (name "go-cloud-google-com-go-websecurityscanner")
    (version "1.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "websecurityscanner"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/websecurityscanner"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2))
    (home-page "https://cloud.google.com/go")
    (synopsis "Web Security Scanner API")
    (description "Go Client Library for Web Security Scanner API.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-auth-oauth2adapt
  (package
    (name "go-cloud-google-com-go-auth-oauth2adapt")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth/oauth2adapt"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "109szg097fn42qpsmrmd29iwsdh2yrjh9krq8mjm02fnm7l18lc4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/auth/oauth2adapt"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-golang-org-x-oauth2
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-auth))
    (home-page "https://cloud.google.com/go")
    (synopsis #f)
    (description "Package oauth2adapt helps converts types used in
@@url{/cloud.google.com/go/auth,cloud.google.com/go/auth} and
@@url{/golang.org/x/oauth2,golang.org/x/oauth2}.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-auth
  (package
    (name "go-cloud-google-com-go-auth")
    (version "0.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "auth"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1srq1a4vyqdr7arvj7ri0p2w3spz63jx46xn9gzcpycgxah7ihkb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/auth"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-golang-org-x-time
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Auth Library for Go")
    (description
     "Package auth provides utilities for managing Google Cloud credentials, including
functionality for creating, caching, and refreshing OAuth2 tokens.  It offers
customizable options for different OAuth2 flows, such as 2-legged (2LO) and
3-legged (3LO) OAuth, along with support for PKCE and automatic token
management.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-iam
  (package
    (name "go-cloud-google-com-go-iam")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iam"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/iam"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "IAM API")
    (description
     "Package iam supports the resource-specific operations of Google Cloud IAM
(Identity and Access Management) for the Google Cloud Libraries.  See
@@url{https://cloud.google.com/iam,https://cloud.google.com/iam} for more about
IAM.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-monitoring
  (package
    (name "go-cloud-google-com-go-monitoring")
    (version "1.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "monitoring"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/monitoring"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Monitoring API")
    (description "Go Client Library for Cloud Monitoring API.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric")
    (version "0.52.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporter/metric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12924v4j592plz62zcq4crxajlm7cyf0ysmaf0mparwvl6b2x7m5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/exporter/metric"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-google-golang-org-genproto-googleapis-api
                             go-go-opentelemetry-io-otel-trace
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-sdk-metric
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-monitoring))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "OpenTelemetry Google Cloud Monitoring Exporter")
    (description
     "@code{OpenTelemetry} Google Cloud Monitoring Exporter allows the user to send
collected metrics to Google Cloud.")
    (license license:asl2.0)))

(define-public go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
  (package
    (name
     "go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12924v4j592plz62zcq4crxajlm7cyf0ysmaf0mparwvl6b2x7m5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go/detectors/gcp"
      #:unpack-path
      "github.com/GoogleCloudPlatform/opentelemetry-operations-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-cloud-google-com-go-compute-metadata))
    (home-page
     "https://github.com/GoogleCloudPlatform/opentelemetry-operations-go")
    (synopsis "GCP Resource detection library")
    (description
     "This is a library intended to be used by Upstream @code{OpenTelemetry} resource
detectors.  It exists within this repository to allow for integration testing of
the detection functions in real GCP environments.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-detectors-gcp
  (package
    (name "go-go-opentelemetry-io-contrib-detectors-gcp")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir "detectors/gcp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/contrib/detectors/gcp"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel
                        go-github-com-stretchr-testify
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-detectors-gcp
                        go-cloud-google-com-go-compute-metadata))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis "GCP Resource detector")
    (description
     "Package gcp provides a resource detector for GCP Cloud Function.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdoutmetric"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdoutmetric"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Metric Exporter")
    (description
     "Package stdoutmetric provides an exporter for @code{OpenTelemetry} metric
telemetry.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-storage
  (package
    (name "go-cloud-google-com-go-storage")
    (version "1.55.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "storage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b2s6zcjckd4hmxlxfybfpi8virdn2siajffp7mcsfvxhj4xypdg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/storage"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-api
                        go-google-golang-org-genproto
                        go-google-golang-org-api
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-stdout-stdoutmetric
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-detectors-gcp
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-googlecloudplatform-opentelemetry-operations-go-exporter-metric
                        go-cloud-google-com-go-longrunning
                        go-cloud-google-com-go-iam
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth
                        go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Storage")
    (description
     "Package storage provides an easy way to work with Google Cloud Storage.  Google
Cloud Storage stores data in named objects, which are grouped into buckets.")
    (license license:asl2.0)))

(define-public go-github-com-google-martian
  (package
    (name "go-github-com-google-martian")
    (version "3.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/martian")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0js95rw72mklxx8dilqdc86a50yhvykyczck4ci3xx6090p3fj2q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/martian/v3"
      #:unpack-path "github.com/google/martian"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc go-golang-org-x-net
                             go-github-com-golang-snappy))
    (home-page "https://github.com/google/martian")
    (synopsis "Martian Proxy")
    (description
     "Package martian provides an HTTP/1.1 proxy with an API for configurable request
and response modifiers.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.121.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dl179xpx9xb0k3s23m49k1mw5krrl20cs6026g7rmz584b54nsh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-api
                             go-golang-org-x-oauth2
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-github-com-googleapis-gax-go-v2
                             go-github-com-google-martian-v3
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-storage))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Client Libraries for Go")
    (description
     "Package cloud is the root of the packages used to access Google Cloud Services.
See
@@url{https://pkg.go.dev/cloud.google.com/go#section-directories,https://pkg.go.dev/cloud.google.com/go#section-directories}
for a full list of sub-modules.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-longrunning
  (package
    (name "go-cloud-google-com-go-longrunning")
    (version "0.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "longrunning"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/longrunning"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "longrunning")
    (description
     "Package longrunning supports Long Running Operations for the Google Cloud
Libraries.  See google.golang.org/genproto/googleapis/longrunning for its
service definition.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-translate
  (package
    (name "go-cloud-google-com-go-translate")
    (version "1.12.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "translate"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/translate"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-golang-org-x-text
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Translation API")
    (description
     "Package translate is the v2 client for the Google Translation API.")
    (license license:asl2.0)))

(define-public go-github-com-google-s2a-go
  (package
    (name "go-github-com-google-s2a-go")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/s2a-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19d2n9w3lm08iiggj9nm4wh64czjbkis3kyvzsy6cqmlyjykch0v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/s2a-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-appengine
                             go-google-golang-org-api
                             go-golang-org-x-sync
                             go-golang-org-x-crypto
                             go-github-com-google-go-cmp
                             go-cloud-google-com-go-translate))
    (home-page "https://github.com/google/s2a-go")
    (synopsis "Secure Session Agent Client Libraries")
    (description
     "Package s2a provides the S2A transport credentials used by a @code{gRPC}
application.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-pkcs11
  (package
    (name "go-github-com-google-go-pkcs11")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-pkcs11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d93v3c0gsrymwagjfp9nzf70yxvczc6kvrqz10w2a07ag5ym02g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-pkcs11"))
    (home-page "https://github.com/google/go-pkcs11")
    (synopsis "Go PKCS #11")
    (description
     "This package provides a Go package for loading PKCS #11 modules.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-enterprise-certificate-proxy
  (package
    (name "go-github-com-googleapis-enterprise-certificate-proxy")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/enterprise-certificate-proxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01y66q708w2vp89gb10iy6vki86hssjwvhia2r0dvwdvbfb9rxi8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/enterprise-certificate-proxy"))
    (propagated-inputs (list go-golang-org-x-sys go-golang-org-x-crypto
                             go-github-com-google-go-pkcs11))
    (home-page "https://github.com/googleapis/enterprise-certificate-proxy")
    (synopsis "Google Proxies for Enterprise Certificates (GA)")
    (description
     "If you use
@@url{https://cloud.google.com/beyondcorp-enterprise/docs/securing-resources-with-certificate-based-access,certificate-based
access} to protect your Google Cloud resources, the end user
@@url{https://en.wikipedia.org/wiki/Client_certificate,device certificate} is
one of the credentials that is verified before access to a resource is granted.
You can configure Google Cloud to use the device certificates in your operating
system key store when verifying access to a resource from the gcloud CLI or
Terraform by using the enterprise certificates feature.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-gax-go
  (package
    (name "go-github-com-googleapis-gax-go")
    (version "2.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/gax-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10lx4d7bw2a9j5ymjwjbn4jnvqmg97p6hjnrdmjwpcgapq2yfmad"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/googleapis/gax-go/v2"
      #:unpack-path "github.com/googleapis/gax-go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/googleapis/gax-go")
    (synopsis #f)
    (description
     "Package gax contains a set of modules which aid the development of APIs for
clients and servers based on @code{gRPC} and Google API conventions.")
    (license license:bsd-3)))

(define-public go-google-golang-org-genproto-googleapis-bytestream
  (package
    (name "go-google-golang-org-genproto-googleapis-bytestream")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/bytestream"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto/googleapis/bytestream"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-api
  (package
    (name "go-google-golang-org-api")
    (version "0.237.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-api-go-client")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ydz4v4j3hwi9745i7l9mac6cjy17xpvir28j7p2l5d08r0jbxrm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/api"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto-googleapis-bytestream
                        go-golang-org-x-time
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-golang-org-x-net
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-github-com-googleapis-gax-go-v2
                        go-github-com-googleapis-enterprise-certificate-proxy
                        go-github-com-google-uuid
                        go-github-com-google-s2a-go
                        go-github-com-google-go-cmp
                        go-cloud-google-com-go-compute-metadata
                        go-cloud-google-com-go-auth-oauth2adapt
                        go-cloud-google-com-go-auth))
    (home-page "https://google.golang.org/api")
    (synopsis "Google APIs Client Library for Go")
    (description
     "Package api is the root of the packages used to access Google Cloud Services.
See
@@url{https://godoc.org/google.golang.org/api,https://godoc.org/google.golang.org/api}
for a full list of sub-packages.")
    (license license:bsd-3)))

(define-public go-cloud-google-com-go-workflows
  (package
    (name "go-cloud-google-com-go-workflows")
    (version "1.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "workflows"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pk121h5dqh7fibzw5341894w4i11drk2ilb6flyxpddi1fsvwy6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cloud.google.com/go/workflows"
      #:unpack-path "cloud.google.com/go"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-genproto
                             go-google-golang-org-api
                             go-github-com-googleapis-gax-go-v2
                             go-cloud-google-com-go-longrunning))
    (home-page "https://cloud.google.com/go")
    (synopsis "Workflows API")
    (description "Go Client Library for Workflows API.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-golang-protobuf
                             go-cloud-google-com-go-workflows
                             go-cloud-google-com-go-websecurityscanner
                             go-cloud-google-com-go-webrisk
                             go-cloud-google-com-go-vpcaccess
                             go-cloud-google-com-go-vmwareengine
                             go-cloud-google-com-go-vmmigration
                             go-cloud-google-com-go-vision-v2
                             go-cloud-google-com-go-videointelligence
                             go-cloud-google-com-go-video
                             go-cloud-google-com-go-translate
                             go-cloud-google-com-go-trace
                             go-cloud-google-com-go-tpu
                             go-cloud-google-com-go-texttospeech
                             go-cloud-google-com-go-talent
                             go-cloud-google-com-go-storagetransfer
                             go-cloud-google-com-go-speech
                             go-cloud-google-com-go-spanner
                             go-cloud-google-com-go-shell
                             go-cloud-google-com-go-servicedirectory
                             go-cloud-google-com-go-securitycenter
                             go-cloud-google-com-go-security
                             go-cloud-google-com-go-secretmanager
                             go-cloud-google-com-go-scheduler
                             go-cloud-google-com-go-run
                             go-cloud-google-com-go-retail
                             go-cloud-google-com-go-resourcesettings
                             go-cloud-google-com-go-resourcemanager
                             go-cloud-google-com-go-redis
                             go-cloud-google-com-go-recommender
                             go-cloud-google-com-go-recommendationengine
                             go-cloud-google-com-go-recaptchaenterprise-v2
                             go-cloud-google-com-go-pubsublite
                             go-cloud-google-com-go-pubsub
                             go-cloud-google-com-go-privatecatalog
                             go-cloud-google-com-go-policytroubleshooter
                             go-cloud-google-com-go-phishingprotection
                             go-cloud-google-com-go-oslogin
                             go-cloud-google-com-go-osconfig
                             go-cloud-google-com-go-orgpolicy
                             go-cloud-google-com-go-orchestration
                             go-cloud-google-com-go-optimization
                             go-cloud-google-com-go-notebooks
                             go-cloud-google-com-go-networksecurity
                             go-cloud-google-com-go-networkmanagement
                             go-cloud-google-com-go-networkconnectivity
                             go-cloud-google-com-go-monitoring
                             go-cloud-google-com-go-metastore
                             go-cloud-google-com-go-memcache
                             go-cloud-google-com-go-mediatranslation
                             go-cloud-google-com-go-maps
                             go-cloud-google-com-go-managedidentities
                             go-cloud-google-com-go-longrunning
                             go-cloud-google-com-go-logging
                             go-cloud-google-com-go-lifesciences
                             go-cloud-google-com-go-language
                             go-cloud-google-com-go-kms
                             go-cloud-google-com-go-iot
                             go-cloud-google-com-go-ids
                             go-cloud-google-com-go-iap
                             go-cloud-google-com-go-iam
                             go-cloud-google-com-go-gsuiteaddons
                             go-cloud-google-com-go-gkemulticloud
                             go-cloud-google-com-go-gkehub
                             go-cloud-google-com-go-gkeconnect
                             go-cloud-google-com-go-gkebackup
                             go-cloud-google-com-go-functions
                             go-cloud-google-com-go-firestore
                             go-cloud-google-com-go-filestore
                             go-cloud-google-com-go-eventarc
                             go-cloud-google-com-go-essentialcontacts
                             go-cloud-google-com-go-errorreporting
                             go-cloud-google-com-go-edgecontainer
                             go-cloud-google-com-go-domains
                             go-cloud-google-com-go-documentai
                             go-cloud-google-com-go-dlp
                             go-cloud-google-com-go-dialogflow
                             go-cloud-google-com-go-deploy
                             go-cloud-google-com-go-datastream
                             go-cloud-google-com-go-datastore
                             go-cloud-google-com-go-dataqna
                             go-cloud-google-com-go-dataproc-v2
                             go-cloud-google-com-go-dataplex
                             go-cloud-google-com-go-datalabeling
                             go-cloud-google-com-go-datafusion
                             go-cloud-google-com-go-dataform
                             go-cloud-google-com-go-dataflow
                             go-cloud-google-com-go-datacatalog
                             go-cloud-google-com-go-containeranalysis
                             go-cloud-google-com-go-container
                             go-cloud-google-com-go-contactcenterinsights
                             go-cloud-google-com-go-compute
                             go-cloud-google-com-go-cloudtasks
                             go-cloud-google-com-go-clouddms
                             go-cloud-google-com-go-cloudbuild
                             go-cloud-google-com-go-channel
                             go-cloud-google-com-go-certificatemanager
                             go-cloud-google-com-go-binaryauthorization
                             go-cloud-google-com-go-billing
                             go-cloud-google-com-go-bigtable
                             go-cloud-google-com-go-bigquery
                             go-cloud-google-com-go-beyondcorp
                             go-cloud-google-com-go-batch
                             go-cloud-google-com-go-baremetalsolution
                             go-cloud-google-com-go-automl
                             go-cloud-google-com-go-assuredworkloads
                             go-cloud-google-com-go-asset
                             go-cloud-google-com-go-artifactregistry
                             go-cloud-google-com-go-area120
                             go-cloud-google-com-go-appengine
                             go-cloud-google-com-go-apigeeregistry
                             go-cloud-google-com-go-apigeeconnect
                             go-cloud-google-com-go-apigateway
                             go-cloud-google-com-go-analytics
                             go-cloud-google-com-go-aiplatform
                             go-cloud-google-com-go-accesscontextmanager
                             go-cloud-google-com-go-accessapproval))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
     "This repository contains the generated Go packages for common protocol buffer
types, and the generated @@url{http://grpc.io,@code{gRPC}} code necessary for
interacting with Google's @code{gRPC} APIs.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-containerd
  (package
    (name "go-github-com-containerd-containerd")
    (version "1.7.27")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16r2xdv13fa1wzgg2lldk34sj67qczr6vp6155yrxnfnfwg09phz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/containerd"))
    (propagated-inputs (list go-tags-cncf-io-container-device-interface
                        go-k8s-io-utils
                        go-k8s-io-klog-v2
                        go-k8s-io-cri-api
                        go-k8s-io-component-base
                        go-k8s-io-client-go
                        go-k8s-io-apiserver
                        go-k8s-io-apimachinery
                        go-k8s-io-api
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-google-golang-org-genproto
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-net
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-bbolt
                        go-github-com-vishvananda-netlink
                        go-github-com-urfave-cli
                        go-github-com-tchap-go-patricia-v2
                        go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-prometheus-client-golang
                        go-github-com-pelletier-go-toml
                        go-github-com-opencontainers-selinux
                        go-github-com-opencontainers-runtime-tools
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-moby-sys-userns
                        go-github-com-moby-sys-user
                        go-github-com-moby-sys-symlink
                        go-github-com-moby-sys-signal
                        go-github-com-moby-sys-sequential
                        go-github-com-moby-sys-mountinfo
                        go-github-com-moby-locker
                        go-github-com-minio-sha256-simd
                        go-github-com-klauspost-compress
                        go-github-com-intel-goresctrl
                        go-github-com-grpc-ecosystem-go-grpc-prometheus
                        go-github-com-grpc-ecosystem-go-grpc-middleware
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-fsnotify-fsnotify
                        go-github-com-emicklei-go-restful-v3
                        go-github-com-docker-go-units
                        go-github-com-docker-go-metrics
                        go-github-com-docker-go-events
                        go-github-com-distribution-reference
                        go-github-com-davecgh-go-spew
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-containernetworking-plugins
                        go-github-com-containernetworking-cni
                        go-github-com-containerd-zfs
                        go-github-com-containerd-typeurl-v2
                        go-github-com-containerd-ttrpc
                        go-github-com-containerd-platforms
                        go-github-com-containerd-nri
                        go-github-com-containerd-log
                        go-github-com-containerd-imgcrypt
                        go-github-com-containerd-go-runc
                        go-github-com-containerd-go-cni
                        go-github-com-containerd-fifo
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-api
                        go-github-com-containerd-console
                        go-github-com-containerd-cgroups-v3
                        go-github-com-containerd-btrfs-v2
                        go-github-com-containerd-aufs
                        go-github-com-microsoft-hcsshim
                        go-github-com-microsoft-go-winio
                        go-github-com-adamkorcz-go-118-fuzz-build
                        go-github-com-adalogics-go-fuzz-headers
                        go-dario-cat-mergo))
    (home-page "https://github.com/containerd/containerd")
    (synopsis "Announcements")
    (description
     "containerd is an industry-standard container runtime with an emphasis on
simplicity, robustness, and portability.  It is available as a daemon for Linux
and Windows, which can manage the complete container lifecycle of its host
system: image transfer and storage, container execution and supervision,
low-level storage and network attachments, etc.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-protobuild
  (package
    (name "go-github-com-containerd-protobuild")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/protobuild")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f44q37qlzh1fkqx4fvhw00fdy191j0253lpjzw5icakjxir3dkp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/protobuild"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc go-golang-org-x-tools
                             go-github-com-pelletier-go-toml
                             go-github-com-golang-protobuf))
    (home-page "https://github.com/containerd/protobuild")
    (synopsis "protobuild")
    (description "Build protobufs in Go, easily.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-containerregistry
  (package
    (name "go-github-com-google-go-containerregistry")
    (version "0.20.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-containerregistry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xz05b3njrpdgc1jry9ky55x32xyr8rkxk5icf5cqycqz54gcsby"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:import-path "github.com/google/go-containerregistry"))
    (propagated-inputs (list go-golang-org-x-tools
                        go-golang-org-x-sync
                        go-golang-org-x-oauth2
                        go-github-com-spf13-cobra
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-moby-docker-image-spec
                        go-github-com-mitchellh-go-homedir
                        go-github-com-klauspost-compress
                        go-github-com-google-go-cmp
                        go-github-com-docker-docker
                        go-github-com-docker-distribution
                        go-github-com-docker-cli
                        go-github-com-containerd-stargz-snapshotter-estargz))
    (home-page "https://github.com/google/go-containerregistry")
    (synopsis "go-containerregistry")
    (description
     "This is a golang library for working with container registries.  It's largely
based on the @@url{https://github.com/google/containerregistry,Python library of
the same name}.")
    (license license:asl2.0)))

(define-public go-github-com-akavel-rsrc
  (package
    (name "go-github-com-akavel-rsrc")
    (version "0.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akavel/rsrc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1frdxqdnk923p4245lk0wwqrxsdy8aj2qxl3m0zfgnh02vfz3hs2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/akavel/rsrc"))
    (home-page "https://github.com/akavel/rsrc")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-josephspurrier-goversioninfo
  (package
    (name "go-github-com-josephspurrier-goversioninfo")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josephspurrier/goversioninfo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07plmi5gy7bl5n5cnymvrirhq2gv3rx6p8ry3kvcdlh7i0x0baj8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/josephspurrier/goversioninfo"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-akavel-rsrc))
    (home-page "https://github.com/josephspurrier/goversioninfo")
    (synopsis "GoVersionInfo")
    (description
     "Package goversioninfo creates a syso file which contains Microsoft Version
Information and an optional icon.")
    (license license:expat)))

(define-public go-github-com-linuxkit-virtsock
  (package
    (name "go-github-com-linuxkit-virtsock")
    (version "0.0.0-20241009230534-cb6a20cc0422")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linuxkit/virtsock")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08f8m6jnq76p1996wbf5kw8s1p9g3ix373rb4f46n9qg1qang4bl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/linuxkit/virtsock"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/linuxkit/virtsock")
    (synopsis "Organisation")
    (description
     "This repository contains Go bindings and sample code for
@@url{https://msdn.microsoft.com/en-us/virtualization/hyperv_on_windows/develop/make_mgmt_service,Hyper-V
sockets} and @@url{http://stefanha.github.io/virtio/,virtio sockets}(VSOCK).")
    (license license:asl2.0)))

(define-public go-github-com-bytecodealliance-wasmtime-go
  (package
    (name "go-github-com-bytecodealliance-wasmtime-go")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bytecodealliance/wasmtime-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqgf8y8zbsfrh8rrrb60n6yrjrf3pqpfady9iam5mq8x9zy978r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bytecodealliance/wasmtime-go/v3"
      #:unpack-path "github.com/bytecodealliance/wasmtime-go"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/bytecodealliance/wasmtime-go")
    (synopsis "Installation")
    (description
     "Package wasmtime is a @code{WebAssembly} runtime for Go powered by Wasmtime.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-stargz-snapshotter-estargz
  (package
    (name "go-github-com-containerd-stargz-snapshotter-estargz")
    (version "0.16.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/stargz-snapshotter")
             (commit (go-version->git-ref version
                                          #:subdir "estargz"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05l361ib00zwwa262y0i970df27knm5dnr89briw97gm2cn0wk6d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/stargz-snapshotter/estargz"
      #:unpack-path "github.com/containerd/stargz-snapshotter"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-vbatts-tar-split
                             go-github-com-opencontainers-go-digest
                             go-github-com-klauspost-compress))
    (home-page "https://github.com/containerd/stargz-snapshotter")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-google-go-intervals
  (package
    (name "go-github-com-google-go-intervals")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-intervals")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v9i46g1vdbyinagj94jvaibw4bpgh2l9f9p5268wg6msf761jm9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/google/go-intervals"))
    (home-page "https://github.com/google/go-intervals")
    (synopsis "go-intervals")
    (description
     "go-intervals is a library for performing set operations on 1-dimensional
intervals, such as time ranges.")
    (license license:asl2.0)))

(define-public go-github-com-magefile-mage
  (package
    (name "go-github-com-magefile-mage")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magefile/mage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zjglw9ra0mc77d6i3yavhihp94qzr9rqx1lhs9whm3qw7gyz4v9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/magefile/mage"))
    (home-page "https://github.com/magefile/mage")
    (synopsis "About")
    (description
     "Mage is a make-like build tool using Go.  You write plain-old go functions, and
Mage automatically uses them as Makefile-like runnable targets.")
    (license license:asl2.0)))

(define-public go-github-com-vbatts-tar-split
  (package
    (name "go-github-com-vbatts-tar-split")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vbatts/tar-split")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13ddyfqzl1mcwm1ja697r7wq1qin4d0hy4yfnpvqwhxddyhxfh9c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/vbatts/tar-split"))
    (propagated-inputs (list go-github-com-urfave-cli
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus
                             go-github-com-magefile-mage
                             go-github-com-fatih-color))
    (home-page "https://github.com/vbatts/tar-split")
    (synopsis "tar-split")
    (description
     "Pristinely disassembling a tar archive, and stashing needed raw bytes and
offsets to reassemble a validating original archive.")
    (license license:bsd-3)))

(define-public go-github-com-containers-storage
  (package
    (name "go-github-com-containers-storage")
    (version "1.58.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/storage")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jff4376yj750586fli0bscy5hdjr4cij02wi5zl22jxlk15l6cr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containers/storage"))
    (propagated-inputs (list go-gotest-tools-v3
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-github-com-vbatts-tar-split
                        go-github-com-ulikunitz-xz
                        go-github-com-tchap-go-patricia-v2
                        go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-opencontainers-selinux
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-moby-sys-user
                        go-github-com-moby-sys-mountinfo
                        go-github-com-moby-sys-capability
                        go-github-com-mistifyio-go-zfs-v3
                        go-github-com-mattn-go-shellwords
                        go-github-com-klauspost-pgzip
                        go-github-com-klauspost-compress
                        go-github-com-json-iterator-go
                        go-github-com-google-go-intervals
                        go-github-com-docker-go-units
                        go-github-com-cyphar-filepath-securejoin
                        go-github-com-containerd-stargz-snapshotter-estargz
                        go-github-com-microsoft-hcsshim
                        go-github-com-microsoft-go-winio
                        go-github-com-burntsushi-toml))
    (home-page "https://github.com/containers/storage")
    (synopsis #f)
    (description
     "@@code{storage} is a Go library which aims to provide methods for storing
filesystem layers, container images, and containers.  A
@@code{containers-storage} CLI wrapper is also included for manual and scripting
use.")
    (license license:asl2.0)))

(define-public go-github-com-checkpoint-restore-checkpointctl
  (package
    (name "go-github-com-checkpoint-restore-checkpointctl")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/checkpointctl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zs54cdf29awvf2mpjs62g649kaxzfh61c9d2prm7bh62pr1qzr0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/checkpointctl"))
    (propagated-inputs (list go-github-com-xlab-treeprint
                             go-github-com-spf13-cobra
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-olekukonko-tablewriter
                             go-github-com-containers-storage
                             go-github-com-checkpoint-restore-go-criu-v7))
    (home-page "https://github.com/checkpoint-restore/checkpointctl")
    (synopsis
     "checkpointctl - a tool for in-depth analysis of container checkpoints")
    (description
     "Container engines like and have the ability to checkpoint a container.  All data
related to a checkpoint is collected in a checkpoint archive.  With the help of
this tool, @@code{checkpointctl}, it is possible to display information about
these checkpoint archives.")
    (license license:asl2.0)))

(define-public go-github-com-checkpoint-restore-go-criu
  (package
    (name "go-github-com-checkpoint-restore-go-criu")
    (version "7.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/go-criu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07qnlmvdzm7gl3lf2kldm83n2bfqnh6qqlhi43734q0dk5bfhpym"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/go-criu/v7"
      #:unpack-path "github.com/checkpoint-restore/go-criu"))
    (propagated-inputs (list go-google-golang-org-protobuf go-golang-org-x-sys
                             go-github-com-spf13-cobra))
    (home-page "https://github.com/checkpoint-restore/go-criu")
    (synopsis "go-criu -- Go bindings for CRIU")
    (description
     "This repository provides Go bindings for @@url{https://criu.org/,CRIU}.  The
code is based on the Go-based PHaul implementation from the CRIU repository.
For easier inclusion into other Go projects, the CRIU Go bindings have been
moved to this repository.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-containerd-api
  (package
    (name "go-github-com-containerd-containerd-api")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (go-version->git-ref version
                                          #:subdir "api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06pl08p1f86z70hy9si5h7p2bgassqjsy625pvzxazxix35ggq6x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/containerd/api"
      #:unpack-path "github.com/containerd/containerd"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-github-com-opencontainers-image-spec
                             go-github-com-containerd-typeurl-v2
                             go-github-com-containerd-ttrpc))
    (home-page "https://github.com/containerd/containerd")
    (synopsis #f)
    (description
     "This directory contains the GRPC API definitions for containerd.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-errdefs-pkg
  (package
    (name "go-github-com-containerd-errdefs-pkg")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/errdefs")
             (commit (go-version->git-ref version
                                          #:subdir "pkg"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0afaljkkd388f6igr3f2vjnd14yr8h20fcfzglw8j5q1q7a1cvik"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/containerd/errdefs/pkg"
      #:unpack-path "github.com/containerd/errdefs"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-github-com-containerd-typeurl-v2
                             go-github-com-containerd-errdefs))
    (home-page "https://github.com/containerd/errdefs")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-containerd-go-cni
  (package
    (name "go-github-com-containerd-go-cni")
    (version "1.1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/go-cni")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yi6r5a48sygfa39wa337ps79f83rfkc1v15vgv3470zan9cfv5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/go-cni"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-sasha-s-go-deadlock
                             go-github-com-containernetworking-cni))
    (home-page "https://github.com/containerd/go-cni")
    (synopsis "go-cni")
    (description
     "This package provides a generic CNI library to provide APIs for CNI plugin
interactions.  The library provides APIs to:.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-go-runc
  (package
    (name "go-github-com-containerd-go-runc")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/go-runc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03f6a44j24g64x0zwx6daqbssbka0wcvj3fkjz4rvqx5dz3n7xhf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/go-runc"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-sirupsen-logrus
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-containerd-console))
    (home-page "https://github.com/containerd/go-runc")
    (synopsis "go-runc")
    (description
     "This is a package for consuming the
@@url{https://github.com/opencontainers/runc,runc} binary in your Go
applications.  It tries to expose all the settings and features of the runc CLI.
If there is something missing then add it, its opensource!")
    (license license:asl2.0)))

(define-public go-github-com-smallstep-pkcs7
  (package
    (name "go-github-com-smallstep-pkcs7")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smallstep/pkcs7")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h8l7aabaxmgzixz4wn0k9f0v4hld86kzis6dpjz2zgf61czk7ri"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/smallstep/pkcs7"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/smallstep/pkcs7")
    (synopsis "pkcs7")
    (description
     "Package pkcs7 implements parsing and generation of some PKCS#7 structures.")
    (license license:expat)))

(define-public go-github-com-stefanberger-go-pkcs11uri
  (package
    (name "go-github-com-stefanberger-go-pkcs11uri")
    (version "0.0.0-20230803200340-78284954bff6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stefanberger/go-pkcs11uri")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05gdfpb2h69fypp91dz1m8mff7mzg5x0kg1vsqh4wwd9cxwdhj10"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/stefanberger/go-pkcs11uri"))
    (home-page "https://github.com/stefanberger/go-pkcs11uri")
    (synopsis "go-pkcs11uri")
    (description
     "Welcome to the go-pkcs11uri library.  The implementation follows
@@url{https://tools.ietf.org/html/rfc7512,RFC 7512} and this
@@url{https://www.rfc-editor.org/errata/rfc7512,errata}.")
    (license license:asl2.0)))

(define-public go-github-com-containers-ocicrypt
  (package
    (name "go-github-com-containers-ocicrypt")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containers/ocicrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hy08qfb68hggw343axaj2k262zv40a7gb464kvp5j0f4qi43p74"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/containers/ocicrypt"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-grpc
                             go-golang-org-x-term
                             go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-stefanberger-go-pkcs11uri
                             go-github-com-smallstep-pkcs7
                             go-github-com-sirupsen-logrus
                             go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-miekg-pkcs11
                             go-github-com-golang-protobuf
                             go-github-com-go-jose-go-jose-v4))
    (home-page "https://github.com/containers/ocicrypt")
    (synopsis "OCIcrypt Library")
    (description
     "The @@code{ocicrypt} library is the OCI image spec implementation of container
image encryption.  More details of the spec can be seen in the
@@url{https://github.com/opencontainers/image-spec/pull/775,OCI repository}.
The purpose of this library is to encode spec structures and consts in code, as
well as provide a consistent implementation of image encryption across container
runtimes and build tools.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-imgcrypt
  (package
    (name "go-github-com-containerd-imgcrypt")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/imgcrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sa0jijbxw24hgxmzkril0skzf5b3kihh8hqkfvm22l7v9c9qg17"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/imgcrypt/v2"
      #:unpack-path "github.com/containerd/imgcrypt"))
    (propagated-inputs (list go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest
                             go-github-com-containers-ocicrypt
                             go-github-com-containerd-typeurl-v2
                             go-github-com-containerd-platforms
                             go-github-com-containerd-errdefs
                             go-github-com-containerd-containerd-v2))
    (home-page "https://github.com/containerd/imgcrypt")
    (synopsis "imgcrypt image encryption library and command line tool")
    (description
     "Project @@code{imgcrypt} is a non-core subproject of containerd.")
    (license license:asl2.0)))

(define-public go-github-com-knqyf263-go-plugin
  (package
    (name "go-github-com-knqyf263-go-plugin")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/knqyf263/go-plugin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c81xa5zcwzbi5r1lf1phh53vpzgc1hq0lymwa741xn9qmj9g0ac"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/knqyf263/go-plugin"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-tetratelabs-wazero
                             go-github-com-stretchr-testify
                             go-github-com-planetscale-vtprotobuf))
    (home-page "https://github.com/knqyf263/go-plugin")
    (synopsis "Go Plugin System over WebAssembly")
    (description
     "@@code{go-plugin} is a Go (golang) plugin system over @code{WebAssembly}
(abbreviated Wasm).  As a plugin is compiled to Wasm, it can be size-efficient,
memory-safe, sandboxed and portable.  The plugin system auto-generates Go SDK
for plugins from
@@url{https://developers.google.com/protocol-buffers/docs/overview,Protocol
Buffers} files.  While it is powered by Wasm, plugin authors/users don't have to
be aware of the Wasm specification since the raw Wasm APIs are capsulated by the
SDK.")
    (license license:expat)))

(define-public go-github-com-containerd-nri
  (package
    (name "go-github-com-containerd-nri")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/nri")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kx35zsawxxbqnyyv3rik89misqcx1ar8d30xndgbbqf2glz73w8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/nri"))
    (propagated-inputs (list go-github-com-opencontainers-runtime-tools
                             go-sigs-k8s-io-yaml
                             go-k8s-io-cri-api
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-golang-org-x-sys
                             go-github-com-tetratelabs-wazero
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo-v2
                             go-github-com-moby-sys-mountinfo
                             go-github-com-knqyf263-go-plugin
                             go-github-com-containerd-ttrpc))
    (home-page "https://github.com/containerd/nri")
    (synopsis "Node Resource Interface, Revisited")
    (description
     "NRI allows plugging domain- or vendor-specific custom logic into OCI- compatible
runtimes.  This logic can make controlled changes to containers or perform extra
actions outside the scope of OCI at certain points in a containers lifecycle.
This can be used, for instance, for improved allocation and management of
devices and other container resources.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-otelttrpc
  (package
    (name "go-github-com-containerd-otelttrpc")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/otelttrpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1npi48pj4g0w1s1wwqky146xc10i4r9dpc5mcgm0nbjpk1f0ixwb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/otelttrpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-containerd-ttrpc))
    (home-page "https://github.com/containerd/otelttrpc")
    (synopsis "ttrpc OpenTelemetry Instrumentation")
    (description
     "package otelttrpc implements Opentelemetry instrumentation support for
@code{ttRPC}.  The package implements unary client and server interceptors for
opentelemetry tracing instrumentation.  The interceptors can be passed as
ttrpc.@code{ClientOpts} and ttrpc.@code{ServerOpt} to @code{ttRPC} during client
and server creation.  The interceptors then automatically handle generating
trace spans for all called and served unary method calls.  If the rest of the
code is properly set up to collect and export tracing data to opentelemetry,
these spans should show up as part of the collected traces.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-ttrpc
  (package
    (name "go-github-com-containerd-ttrpc")
    (version "1.2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/ttrpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fcd9nqkj1iz95h16i8ygiiazvbzmgrawpnvqcpbfhjwkx3sc1m1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/ttrpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-golang-org-x-sys
                             go-github-com-prometheus-procfs
                             go-github-com-golang-protobuf
                             go-github-com-gogo-protobuf
                             go-github-com-containerd-log))
    (home-page "https://github.com/containerd/ttrpc")
    (synopsis "ttrpc")
    (description
     "package ttrpc defines and implements a low level simple transfer protocol
optimized for low latency and reliable connections between processes on the same
host.  The protocol uses simple framing for sending requests, responses, and
data using multiple streams.")
    (license license:asl2.0)))

(define-public go-github-com-mistifyio-go-zfs
  (package
    (name "go-github-com-mistifyio-go-zfs")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mistifyio/go-zfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19wasizw95x1dsc5wxqsxc3knbnkbk3xpha6l506dc5a7wjfs8ir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mistifyio/go-zfs/v3"
      #:unpack-path "github.com/mistifyio/go-zfs"))
    (propagated-inputs (list go-github-com-google-uuid))
    (home-page "https://github.com/mistifyio/go-zfs")
    (synopsis "Go Wrapper for ZFS")
    (description
     "Package zfs provides wrappers around the ZFS command line tools.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-zfs
  (package
    (name "go-github-com-containerd-zfs")
    (version "2.0.0-rc.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/zfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0448z4k61kp3dv97fj8sf8ghgb2zrwfpikb22izh8qns2ixj9pzy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/zfs/v2"
      #:unpack-path "github.com/containerd/zfs"))
    (propagated-inputs (list go-github-com-mistifyio-go-zfs-v3
                             go-github-com-containerd-plugin
                             go-github-com-containerd-platforms
                             go-github-com-containerd-log
                             go-github-com-containerd-continuity
                             go-github-com-containerd-containerd-v2))
    (home-page "https://github.com/containerd/zfs")
    (synopsis "ZFS snapshotter plugin")
    (description "ZFS snapshotter plugin for containerd.")
    (license license:asl2.0)))

(define-public go-github-com-docker-go-events
  (package
    (name "go-github-com-docker-go-events")
    (version "0.0.0-20250114142523-c867878c5e32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-events")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08gsifx7lrgq6h8w6sgys1d09gvjyw8flback32r0ryh64z125xm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/go-events"))
    (home-page "https://github.com/docker/go-events")
    (synopsis "Docker Events Package")
    (description
     "The Docker @@code{events} package implements a composable event distribution
package for Go.")
    (license license:asl2.0)))

(define-public go-github-com-docker-go-metrics
  (package
    (name "go-github-com-docker-go-metrics")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/docker/go-metrics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b6f1889chmwlsgrqxylnks2jic16j2dqhsdd1dvaklk48ky95ga"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/go-metrics"))
    (propagated-inputs (list go-github-com-prometheus-client-golang))
    (home-page "https://github.com/docker/go-metrics")
    (synopsis "go-metrics")
    (description
     "This package is small wrapper around the prometheus go client to help enforce
convention and best practices for metrics collection in Docker projects.")
    (license (list license:asl2.0 license:cc-by-sa4.0))))

(define-public go-github-com-intel-goresctrl
  (package
    (name "go-github-com-intel-goresctrl")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/goresctrl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "164qjx8lnjmb8c1vx1z80wyi1s8p03vglq1xfhszi7p88mbgmiwh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/intel/goresctrl"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-k8s-io-apimachinery
                             go-golang-org-x-sys
                             go-golang-org-x-exp
                             go-github-com-prometheus-client-golang
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/intel/goresctrl")
    (synopsis "Go Resource Control")
    (description
     "The goresctrl library provides Go interface to manage following resources.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-symlink
  (package
    (name "go-github-com-moby-sys-symlink")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "symlink"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/symlink"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description
     "Package symlink implements
@@url{#@code{FollowSymlinkInScope,FollowSymlinkInScope}} which is an extension
of @@url{/path/filepath#@code{EvalSymlinks,path/filepath.EvalSymlinks}}, as well
as a Windows long-path aware version of
@@url{/path/filepath#@code{EvalSymlinks,path/filepath.EvalSymlinks}} from the Go
standard library.")
    (license license:asl2.0)))

(define-public go-github-com-grpc-ecosystem-go-grpc-prometheus
  (package
    (name "go-github-com-grpc-ecosystem-go-grpc-prometheus")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/go-grpc-prometheus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lzk54h7np32b3acidg1ggbn8ppbnns0m71gcg9d1qkkdh8zrijl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/grpc-ecosystem/go-grpc-prometheus"))
    (home-page "https://github.com/grpc-ecosystem/go-grpc-prometheus")
    (synopsis "Go gRPC Interceptors for Prometheus monitoring")
    (description
     "@@url{https://prometheus.io/,Prometheus} monitoring for your
@@url{https://github.com/grpc/grpc-go,@code{gRPC} Go} servers and clients.")
    (license license:asl2.0)))

(define-public go-go-etcd-io-etcd-api
  (package
    (name "go-go-etcd-io-etcd-api")
    (version "3.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version
                                          #:subdir "api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crs8ixa7rn2vi30l8hqi30jyih7b17qrp6kn49kv7x177g45jrh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.etcd.io/etcd/api/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-stretchr-testify
                             go-github-com-grpc-ecosystem-grpc-gateway-v2
                             go-github-com-golang-protobuf
                             go-github-com-gogo-protobuf
                             go-github-com-coreos-go-semver))
    (home-page "https://go.etcd.io/etcd")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-go-etcd-io-etcd-client-pkg
  (package
    (name "go-go-etcd-io-etcd-client-pkg")
    (version "3.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version
                                          #:subdir "client/pkg"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crs8ixa7rn2vi30l8hqi30jyih7b17qrp6kn49kv7x177g45jrh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.etcd.io/etcd/client/pkg/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (propagated-inputs (list go-golang-org-x-sys go-go-uber-org-zap
                             go-github-com-stretchr-testify
                             go-github-com-coreos-go-systemd-v22))
    (home-page "https://go.etcd.io/etcd")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-go-etcd-io-etcd-client
  (package
    (name "go-go-etcd-io-etcd-client")
    (version "3.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version
                                          #:subdir "client"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crs8ixa7rn2vi30l8hqi30jyih7b17qrp6kn49kv7x177g45jrh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.etcd.io/etcd/client/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                        go-google-golang-org-grpc
                        go-go-uber-org-zap
                        go-github-com-stretchr-testify
                        go-github-com-prometheus-client-golang
                        go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
                        go-github-com-dustin-go-humanize
                        go-github-com-coreos-go-semver))
    (home-page "https://go.etcd.io/etcd")
    (synopsis "etcd/client/v3")
    (description
     "Package clientv3 implements the official Go etcd client for v3.")
    (license license:asl2.0)))

(define-public go-github-com-grpc-ecosystem-go-grpc-middleware
  (package
    (name "go-github-com-grpc-ecosystem-go-grpc-middleware")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/go-grpc-middleware")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04g3yw8ywfjcgg6rli0vij2z6b9dd5vpsh39l33ysnr6zdrb76np"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/grpc-ecosystem/go-grpc-middleware"))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-golang-org-x-oauth2
                             go-golang-org-x-net
                             go-go-uber-org-zap
                             go-github-com-stretchr-testify
                             go-github-com-sirupsen-logrus
                             go-github-com-opentracing-opentracing-go
                             go-github-com-golang-protobuf
                             go-github-com-gogo-protobuf
                             go-github-com-go-kit-log))
    (home-page "https://github.com/grpc-ecosystem/go-grpc-middleware")
    (synopsis "Go gRPC Middleware")
    (description
     "`grpc_middleware` is a collection of @code{gRPC} middleware packages:
interceptors, helpers and tools.")
    (license license:asl2.0)))

(define-public go-github-com-antlr4-go-antlr
  (package
    (name "go-github-com-antlr4-go-antlr")
    (version "4.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/antlr4-go/antlr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m5q00fvz28dgvv3ws924p6gamxm6gzqfm12f5ryhljifg22xq3d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/antlr4-go/antlr/v4"
      #:unpack-path "github.com/antlr4-go/antlr"))
    (propagated-inputs (list go-golang-org-x-exp))
    (home-page "https://github.com/antlr4-go/antlr")
    (synopsis "ANTLR4 Go Runtime Module Repo")
    (description
     "Package antlr implements the Go version of the ANTLR 4 runtime.")
    (license license:bsd-3)))

(define-public go-buf-build-go-protovalidate
  (package
    (name "go-buf-build-go-protovalidate")
    (version "0.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bufbuild/protovalidate-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1igkgb5af29nqlab501nx2dwnw0y9hyhn6a11ykyw9q76mph9qgj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "buf.build/go/protovalidate"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-github-com-stretchr-testify
                        go-github-com-google-cel-go
                        go-buf-build-gen-go-bufbuild-protovalidate-protocolbuffers-go))
    (home-page "https://buf.build/go/protovalidate")
    (synopsis "protovalidate-go")
    (description
     "@@url{https://buf.build/docs/protovalidate/,Protovalidate} provides standard
annotations to validate common rules on messages and fields, as well as the
ability to use @@url{https://cel.dev,CEL} to write custom rules.  It's the next
generation of
@@url{https://github.com/bufbuild/protoc-gen-validate,protoc-gen-validate}, the
only widely used validation library for Protobuf.")
    (license license:asl2.0)))

(define-public go-github-com-grpc-ecosystem-go-grpc-middleware
  (package
    (name "go-github-com-grpc-ecosystem-go-grpc-middleware")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/go-grpc-middleware")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ln71gclxw9wi23ai2ph8rzdj1mpb0zj1z1g7y0ivk6c9a93bl98"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/grpc-ecosystem/go-grpc-middleware/v2"
      #:unpack-path "github.com/grpc-ecosystem/go-grpc-middleware"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-golang-org-x-oauth2
                        go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-buf-build-go-protovalidate
                        go-buf-build-gen-go-bufbuild-protovalidate-protocolbuffers-go))
    (home-page "https://github.com/grpc-ecosystem/go-grpc-middleware")
    (synopsis "Go gRPC Middleware")
    (description "Package middleware.")
    (license license:asl2.0)))

(define-public go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
  (package
    (name
     "go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/go-grpc-middleware")
             (commit (go-version->git-ref version
                                          #:subdir "providers/prometheus"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16yp2znijgk7hws6dx2g1xfj8mlmw8irs04424sacg4v58byj0lv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/grpc-ecosystem/go-grpc-middleware/providers/prometheus"
      #:unpack-path "github.com/grpc-ecosystem/go-grpc-middleware"))
    (propagated-inputs (list go-google-golang-org-grpc
                        go-github-com-stretchr-testify
                        go-github-com-prometheus-client-model
                        go-github-com-prometheus-client-golang
                        go-github-com-grpc-ecosystem-go-grpc-middleware-v2))
    (home-page "https://github.com/grpc-ecosystem/go-grpc-middleware")
    (synopsis #f)
    (description
     "Package prometheus provides a standalone interceptor for metrics.  It's next
iteration of deprecated
@@url{https://github.com/grpc-ecosystem/go-grpc-prometheus,https://github.com/grpc-ecosystem/go-grpc-prometheus}.
 See
@@url{https://github.com/grpc-ecosystem/go-grpc-middleware/tree/main/examples,https://github.com/grpc-ecosystem/go-grpc-middleware/tree/main/examples}
for example.")
    (license license:asl2.0)))

(define-public go-github-com-soheilhy-cmux
  (package
    (name "go-github-com-soheilhy-cmux")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/soheilhy/cmux")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17zpa45xvmk3jl1qqx3lkqdyxs6hdczpv7hwg4s7x0895jx3zl5y"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/soheilhy/cmux"))
    (propagated-inputs (list go-golang-org-x-net))
    (home-page "https://github.com/soheilhy/cmux")
    (synopsis "cmux: Connection Mux")
    (description
     "Package cmux is a library to multiplex network connections based on their
payload.  Using cmux, you can serve different protocols from the same listener.")
    (license license:asl2.0)))

(define-public go-github-com-tmc-grpc-websocket-proxy
  (package
    (name "go-github-com-tmc-grpc-websocket-proxy")
    (version "0.0.0-20220101234140-673ab2c3ae75")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tmc/grpc-websocket-proxy")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0299q3njcs6smrac6734xfjslpvwvsc8jyh5fq5849cdlp5a8290"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tmc/grpc-websocket-proxy"))
    (propagated-inputs (list go-golang-org-x-net go-github-com-sirupsen-logrus
                             go-github-com-gorilla-websocket))
    (home-page "https://github.com/tmc/grpc-websocket-proxy")
    (synopsis "grpc-websocket-proxy")
    (description
     "Wrap your grpc-gateway mux with this helper to expose streaming endpoints over
websockets.")
    (license license:expat)))

(define-public go-github-com-xiang90-probing
  (package
    (name "go-github-com-xiang90-probing")
    (version "0.0.0-20221125231312-a49e3df8f510")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xiang90/probing")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w0pwy8fzxfqnsjzl28jy08di58kwag48vlj4yj9hls77qgbr9ar"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xiang90/probing"))
    (home-page "https://github.com/xiang90/probing")
    (synopsis "Getting Started")
    (description "We first need to serve the probing HTTP handler.")
    (license license:expat)))

(define-public go-github-com-cockroachdb-datadriven
  (package
    (name "go-github-com-cockroachdb-datadriven")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cockroachdb/datadriven")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h2dhm4g1n8ny71lm495l5l842lwq320la0q8b8zmr4w3s5nyk09"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cockroachdb/datadriven"))
    (propagated-inputs (list go-github-com-pmezard-go-difflib))
    (home-page "https://github.com/cockroachdb/datadriven")
    (synopsis "Data-Driven Tests for Go")
    (description
     "This repository implements an extension of
@@url{https://github.com/golang/go/wiki/@code{TableDrivenTests,Table-Driven}
Testing}.  Instead of building and iterating over a table in the test code, the
input is further separated into files (or inline strings).  For certain classes
of tests, this can significantly reduce the friction involved in writing and
reading these tests.")
    (license license:asl2.0)))

(define-public go-go-etcd-io-raft
  (package
    (name "go-go-etcd-io-raft")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/raft")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18d9l8739irzgl0860qrrmdn3aqhc92lhc9brv8lkq7n59gxqd6i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.etcd.io/raft/v3"
      #:unpack-path "go.etcd.io/raft"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-golang-protobuf
                             go-github-com-gogo-protobuf
                             go-github-com-cockroachdb-datadriven))
    (home-page "https://go.etcd.io/raft")
    (synopsis "Raft library")
    (description
     "Package raft sends and receives messages in the Protocol Buffer format defined
in the raftpb package.")
    (license license:asl2.0)))

(define-public go-go-etcd-io-etcd-server
  (package
    (name "go-go-etcd-io-etcd-server")
    (version "3.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/etcd-io/etcd")
             (commit (go-version->git-ref version
                                          #:subdir "server"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crs8ixa7rn2vi30l8hqi30jyih7b17qrp6kn49kv7x177g45jrh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.etcd.io/etcd/server/v3"
      #:unpack-path "go.etcd.io/etcd"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                        go-sigs-k8s-io-json
                        go-gopkg-in-natefinch-lumberjack-v2
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-api
                        go-golang-org-x-time
                        go-golang-org-x-net
                        go-golang-org-x-crypto
                        go-go-uber-org-zap
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-raft-v3
                        go-go-etcd-io-bbolt
                        go-github-com-xiang90-probing
                        go-github-com-tmc-grpc-websocket-proxy
                        go-github-com-stretchr-testify
                        go-github-com-spf13-cobra
                        go-github-com-soheilhy-cmux
                        go-github-com-prometheus-client-model
                        go-github-com-prometheus-client-golang
                        go-github-com-jonboulle-clockwork
                        go-github-com-grpc-ecosystem-grpc-gateway-v2
                        go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
                        go-github-com-grpc-ecosystem-go-grpc-middleware
                        go-github-com-google-go-cmp
                        go-github-com-google-btree
                        go-github-com-golang-protobuf
                        go-github-com-golang-groupcache
                        go-github-com-golang-jwt-jwt-v5
                        go-github-com-gogo-protobuf
                        go-github-com-dustin-go-humanize
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-coreos-go-semver))
    (home-page "https://go.etcd.io/etcd")
    (synopsis #f)
    (description
     "Package main is a simple wrapper of the real etcd entrypoint package (located at
go.etcd.io/etcd/etcdmain) to ensure that etcd is still \"go getable\"; e.g. `go
get go.etcd.io/etcd` works as expected and builds a binary in $GOBIN/etcd.")
    (license license:asl2.0)))

(define-public go-k8s-io-kms
  (package
    (name "go-k8s-io-kms")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/kms")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0syd4655b9s3m39kj3jn917za5318r3fw1xqbx48cbz4w48l7xic"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/kms"))
    (propagated-inputs (list go-google-golang-org-grpc
                             go-github-com-gogo-protobuf))
    (home-page "https://k8s.io/kms")
    (synopsis "KMS")
    (description "Package kms contains the proto definitions for the kms API.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client
  (package
    (name "go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client")
    (version "0.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/apiserver-network-proxy")
             (commit (go-version->git-ref version
                                          #:subdir "konnectivity-client"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1917xk4cqwrhk1knyqjkrxndahhdssm0w2zyasj5riqjvdxnjw06"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "sigs.k8s.io/apiserver-network-proxy/konnectivity-client"
      #:unpack-path "sigs.k8s.io/apiserver-network-proxy"))
    (propagated-inputs (list go-k8s-io-klog-v2 go-google-golang-org-protobuf
                             go-google-golang-org-grpc go-go-uber-org-goleak
                             go-github-com-prometheus-client-golang))
    (home-page "https://sigs.k8s.io/apiserver-network-proxy")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-k8s-io-apiserver
  (package
    (name "go-k8s-io-apiserver")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/apiserver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08v3gypdy3vva06xy9vjy2dm1nb1ii6hvxgq9w3d206hhg5al7yk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/apiserver"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                        go-sigs-k8s-io-structured-merge-diff-v4
                        go-sigs-k8s-io-randfill
                        go-sigs-k8s-io-json
                        go-sigs-k8s-io-apiserver-network-proxy-konnectivity-client
                        go-k8s-io-utils
                        go-k8s-io-kube-openapi
                        go-k8s-io-kms
                        go-k8s-io-klog-v2
                        go-k8s-io-component-base
                        go-k8s-io-client-go
                        go-k8s-io-apimachinery
                        go-k8s-io-api
                        go-gopkg-in-natefinch-lumberjack-v2
                        go-gopkg-in-go-jose-go-jose-v2
                        go-gopkg-in-evanphx-json-patch-v4
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-api
                        go-golang-org-x-time
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-net
                        go-golang-org-x-crypto
                        go-go-uber-org-zap
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-metric
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-etcd-server-v3
                        go-go-etcd-io-etcd-client-v3
                        go-go-etcd-io-etcd-client-pkg-v3
                        go-go-etcd-io-etcd-api-v3
                        go-github-com-stretchr-testify
                        go-github-com-spf13-pflag
                        go-github-com-mxk-go-flowrate
                        go-github-com-munnerz-goautoneg
                        go-github-com-grpc-ecosystem-go-grpc-prometheus
                        go-github-com-gorilla-websocket
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-google-gnostic-models
                        go-github-com-google-cel-go
                        go-github-com-google-btree
                        go-github-com-gogo-protobuf
                        go-github-com-go-logr-logr
                        go-github-com-fsnotify-fsnotify
                        go-github-com-emicklei-go-restful-v3
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-coreos-go-oidc
                        go-github-com-blang-semver-v4))
    (home-page "https://k8s.io/apiserver")
    (synopsis "apiserver")
    (description
     "Generic library for building a Kubernetes aggregated API server.")
    (license license:asl2.0)))

(define-public go-github-com-go-logr-zapr
  (package
    (name "go-github-com-go-logr-zapr")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-logr/zapr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07kg9h853jijfc86zm07856sisac6jwvn06gbk694fg00mj1806f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-logr/zapr"))
    (propagated-inputs (list go-go-uber-org-zap go-github-com-stretchr-testify
                             go-github-com-go-logr-logr))
    (home-page "https://github.com/go-logr/zapr")
    (synopsis "Zapr ⚡")
    (description
     "Package zapr defines an implementation of the github.com/go-logr/logr interfaces
built on top of Zap (go.uber.org/zap).")
    (license license:asl2.0)))

(define-public go-github-com-azure-go-ansiterm
  (package
    (name "go-github-com-azure-go-ansiterm")
    (version "0.0.0-20250102033503-faa5f7b0171c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/go-ansiterm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11jwgg6zbwkij8vqcmia8pjk5s272f3zc89hh3g6v7k3vhk0lrp1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Azure/go-ansiterm"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/Azure/go-ansiterm")
    (synopsis "go-ansiterm")
    (description
     "This is a cross platform Ansi Terminal Emulation library.  It reads a stream of
Ansi characters and produces the appropriate function calls.  The results of the
function calls are platform dependent.")
    (license license:expat)))

(define-public go-github-com-moby-term
  (package
    (name "go-github-com-moby-term")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/term")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05g3dn1hbk9vxzp3dm752j8zn1gy61qzxm33nsj7xisa8s6v8vgw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/term"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-creack-pty
                             go-github-com-azure-go-ansiterm))
    (home-page "https://github.com/moby/term")
    (synopsis "term - utilities for dealing with terminals")
    (description
     "Package term provides structures and helper functions to work with terminal
(state, sizes).")
    (license license:asl2.0)))

(define-public go-k8s-io-api
  (package
    (name "go-k8s-io-api")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/api")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1451by85qkxwa5zckl029rmr9lwrk88cdxspsmhi5j5in7dg1znv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/api"))
    (propagated-inputs (list go-k8s-io-apimachinery
                             go-github-com-gogo-protobuf))
    (home-page "https://k8s.io/api")
    (synopsis "api")
    (description
     "Schema of the external API types that are served by the Kubernetes API server.")
    (license license:asl2.0)))

(define-public go-github-com-moby-spdystream
  (package
    (name "go-github-com-moby-spdystream")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/spdystream")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p5pwwspmp24ff900656fyvrgdz8xxl6y0dk9fqgcaaaylmw0v9g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/spdystream"))
    (home-page "https://github.com/moby/spdystream")
    (synopsis "SpdyStream")
    (description
     "This package provides a multiplexed stream library using spdy.")
    (license license:asl2.0)))

(define-public go-github-com-mxk-go-flowrate
  (package
    (name "go-github-com-mxk-go-flowrate")
    (version "0.0.0-20140419014527-cca7078d478f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mxk/go-flowrate")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zqs39923ja0yypdmiqk6x8pgmfs3ms5x5sl1dqv9z6zyx2xy541"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mxk/go-flowrate"))
    (home-page "https://github.com/mxk/go-flowrate")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-k8s-io-apimachinery
  (package
    (name "go-k8s-io-apimachinery")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/apimachinery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r633vzbbfy9s9a32f0l6jnmnf7lli0wzk7qpsz9nq55axx7q38j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/apimachinery"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-sigs-k8s-io-structured-merge-diff-v4
                             go-sigs-k8s-io-randfill
                             go-sigs-k8s-io-json
                             go-k8s-io-utils
                             go-k8s-io-kube-openapi
                             go-k8s-io-klog-v2
                             go-gopkg-in-inf-v0
                             go-gopkg-in-evanphx-json-patch-v4
                             go-golang-org-x-time
                             go-golang-org-x-net
                             go-github-com-stretchr-testify
                             go-github-com-spf13-pflag
                             go-github-com-mxk-go-flowrate
                             go-github-com-moby-spdystream
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-google-gnostic-models
                             go-github-com-gogo-protobuf
                             go-github-com-fxamacker-cbor-v2
                             go-github-com-davecgh-go-spew
                             go-github-com-armon-go-socks5))
    (home-page "https://k8s.io/apimachinery")
    (synopsis "apimachinery")
    (description
     "Scheme, typing, encoding, decoding, and conversion packages for Kubernetes and
Kubernetes-like API objects.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-randfill
  (package
    (name "go-sigs-k8s-io-randfill")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/randfill")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nk0vk269jw0k155yna43jx9lz4dbzhlhp98jrk8iwqn7k250my6"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "sigs.k8s.io/randfill"))
    (home-page "https://sigs.k8s.io/randfill")
    (synopsis "randfill")
    (description
     "Package randfill is a library for populating go objects with random values.")
    (license license:asl2.0)))

(define-public go-k8s-io-client-go
  (package
    (name "go-k8s-io-client-go")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/client-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k7qgrd0111hgqcs0rbwlpwkg3vn4ahfaz0n5ccy7s92x3yrxlig"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/client-go"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-sigs-k8s-io-structured-merge-diff-v4
                             go-sigs-k8s-io-randfill
                             go-sigs-k8s-io-json
                             go-k8s-io-utils
                             go-k8s-io-kube-openapi
                             go-k8s-io-klog-v2
                             go-k8s-io-apimachinery
                             go-k8s-io-api
                             go-gopkg-in-evanphx-json-patch-v4
                             go-google-golang-org-protobuf
                             go-golang-org-x-time
                             go-golang-org-x-term
                             go-golang-org-x-oauth2
                             go-golang-org-x-net
                             go-go-uber-org-goleak
                             go-github-com-stretchr-testify
                             go-github-com-spf13-pflag
                             go-github-com-peterbourgon-diskv
                             go-github-com-munnerz-goautoneg
                             go-github-com-gregjones-httpcache
                             go-github-com-gorilla-websocket
                             go-github-com-google-uuid
                             go-github-com-google-go-cmp
                             go-github-com-google-gnostic-models
                             go-github-com-gogo-protobuf
                             go-github-com-go-logr-logr))
    (home-page "https://k8s.io/client-go")
    (synopsis "client-go")
    (description
     "Go clients for talking to a @@url{http://kubernetes.io/,kubernetes} cluster.")
    (license license:asl2.0)))

(define-public go-k8s-io-component-base
  (package
    (name "go-k8s-io-component-base")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/component-base")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107kkzzfd5d302d26gyzqil54m9ava39w09lfiy6hxrs9iwk0fqc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/component-base"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                        go-sigs-k8s-io-json
                        go-k8s-io-utils
                        go-k8s-io-klog-v2
                        go-k8s-io-client-go
                        go-k8s-io-apimachinery
                        go-golang-org-x-sys
                        go-go-uber-org-zap
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-github-com-stretchr-testify
                        go-github-com-spf13-pflag
                        go-github-com-spf13-cobra
                        go-github-com-prometheus-procfs
                        go-github-com-prometheus-common
                        go-github-com-prometheus-client-model
                        go-github-com-prometheus-client-golang
                        go-github-com-munnerz-goautoneg
                        go-github-com-moby-term
                        go-github-com-google-go-cmp
                        go-github-com-go-logr-zapr
                        go-github-com-go-logr-logr
                        go-github-com-blang-semver-v4))
    (home-page "https://k8s.io/component-base")
    (synopsis "component-base")
    (description
     "Implement KEP 32:
@@url{https://github.com/kubernetes/enhancements/blob/master/keps/sig-cluster-lifecycle/wgs/783-component-base/README.md,https://github.com/kubernetes/enhancements/blob/master/keps/sig-cluster-lifecycle/wgs/783-component-base/README.md}.")
    (license license:asl2.0)))

(define-public go-k8s-io-kubelet
  (package
    (name "go-k8s-io-kubelet")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes/kubelet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17c308n1dc4ngbgw8h7l2hcnpma6wic1qvlsfldsysc67k0nxzqr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "k8s.io/kubelet"))
    (propagated-inputs (list go-k8s-io-utils
                             go-k8s-io-klog-v2
                             go-k8s-io-cri-api
                             go-k8s-io-component-base
                             go-k8s-io-client-go
                             go-k8s-io-apiserver
                             go-k8s-io-apimachinery
                             go-k8s-io-api
                             go-google-golang-org-grpc
                             go-github-com-stretchr-testify
                             go-github-com-gogo-protobuf
                             go-github-com-emicklei-go-restful-v3))
    (home-page "https://k8s.io/kubelet")
    (synopsis "kubelet")
    (description
     "This package implements
@@url{https://git.k8s.io/enhancements/keps/sig-cluster-lifecycle/wgs/115-componentconfig/README.md#kubelet-changes,KEP
14 - Moving @code{ComponentConfig} API types to staging repos}.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-containerd
  (package
    (name "go-github-com-containerd-containerd")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/containerd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hi20jmj20kjgxf4zblx8ykwiyph0f8gslid920hj4nwsvdma59z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containerd/containerd/v2"
      #:unpack-path "github.com/containerd/containerd"))
    (propagated-inputs (list go-tags-cncf-io-container-device-interface
                        go-k8s-io-utils
                        go-k8s-io-kubelet
                        go-k8s-io-klog-v2
                        go-k8s-io-cri-api
                        go-k8s-io-client-go
                        go-k8s-io-apimachinery
                        go-gopkg-in-inf-v0
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-mod
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-bbolt
                        go-github-com-vishvananda-netns
                        go-github-com-vishvananda-netlink
                        go-github-com-urfave-cli-v2
                        go-github-com-tchap-go-patricia-v2
                        go-github-com-stretchr-testify
                        go-github-com-sirupsen-logrus
                        go-github-com-prometheus-client-golang
                        go-github-com-pelletier-go-toml-v2
                        go-github-com-opencontainers-selinux
                        go-github-com-opencontainers-runtime-tools
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-moby-sys-userns
                        go-github-com-moby-sys-user
                        go-github-com-moby-sys-symlink
                        go-github-com-moby-sys-signal
                        go-github-com-moby-sys-sequential
                        go-github-com-moby-sys-mountinfo
                        go-github-com-moby-locker
                        go-github-com-mdlayher-vsock
                        go-github-com-klauspost-compress
                        go-github-com-intel-goresctrl
                        go-github-com-grpc-ecosystem-go-grpc-middleware-providers-prometheus
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-fsnotify-fsnotify
                        go-github-com-docker-go-units
                        go-github-com-docker-go-metrics
                        go-github-com-docker-go-events
                        go-github-com-distribution-reference
                        go-github-com-davecgh-go-spew
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-containernetworking-plugins
                        go-github-com-containernetworking-cni
                        go-github-com-containerd-zfs-v2
                        go-github-com-containerd-typeurl-v2
                        go-github-com-containerd-ttrpc
                        go-github-com-containerd-plugin
                        go-github-com-containerd-platforms
                        go-github-com-containerd-otelttrpc
                        go-github-com-containerd-nri
                        go-github-com-containerd-log
                        go-github-com-containerd-imgcrypt-v2
                        go-github-com-containerd-go-runc
                        go-github-com-containerd-go-cni
                        go-github-com-containerd-fifo
                        go-github-com-containerd-errdefs-pkg
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-api
                        go-github-com-containerd-console
                        go-github-com-containerd-cgroups-v3
                        go-github-com-containerd-btrfs-v2
                        go-github-com-checkpoint-restore-go-criu-v7
                        go-github-com-checkpoint-restore-checkpointctl
                        go-github-com-microsoft-hcsshim
                        go-github-com-microsoft-go-winio
                        go-github-com-adalogics-go-fuzz-headers
                        go-dario-cat-mergo))
    (home-page "https://github.com/containerd/containerd")
    (synopsis "Announcements")
    (description
     "containerd is an industry-standard container runtime with an emphasis on
simplicity, robustness, and portability.  It is available as a daemon for Linux
and Windows, which can manage the complete container lifecycle of its host
system: image transfer and storage, container execution and supervision,
low-level storage and network attachments, etc.")
    (license license:asl2.0)))

(define-public go-github-com-containerd-errdefs
  (package
    (name "go-github-com-containerd-errdefs")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containerd/errdefs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0afaljkkd388f6igr3f2vjnd14yr8h20fcfzglw8j5q1q7a1cvik"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containerd/errdefs"))
    (home-page "https://github.com/containerd/errdefs")
    (synopsis "errdefs")
    (description
     "Package errdefs defines the common errors used throughout containerd packages.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-zpages
  (package
    (name "go-go-opentelemetry-io-contrib-zpages")
    (version "0.61.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                                          #:subdir "zpages"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/contrib/zpages"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description
     "Package zpages implements a collection of HTML pages that display telemetry
stats.")
    (license license:asl2.0)))

(define-public go-github-com-dgraph-io-badger
  (package
    (name "go-github-com-dgraph-io-badger")
    (version "4.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hypermodeinc/badger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lk1446bx4q9y2l2qggjxxim787wj1lrshakngd362ifjn3dm2a7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/dgraph-io/badger/v4"
      #:unpack-path "github.com/dgraph-io/badger"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-golang-org-x-sys
                             go-golang-org-x-net
                             go-go-opentelemetry-io-otel
                             go-go-opentelemetry-io-contrib-zpages
                             go-github-com-stretchr-testify
                             go-github-com-spf13-cobra
                             go-github-com-klauspost-compress
                             go-github-com-google-flatbuffers
                             go-github-com-dustin-go-humanize
                             go-github-com-dgraph-io-ristretto-v2
                             go-github-com-cespare-xxhash-v2))
    (home-page "https://github.com/dgraph-io/badger")
    (synopsis "BadgerDB")
    (description
     "Package badger implements an embeddable, simple and fast key-value database,
written in pure Go.  It is designed to be highly performant for both reads and
writes simultaneously.  Badger uses Multi-Version Concurrency Control (MVCC),
and supports transactions.  It runs transactions concurrently, with serializable
snapshot isolation guarantees.")
    (license license:asl2.0)))

(define-public go-github-com-fortytw2-leaktest
  (package
    (name "go-github-com-fortytw2-leaktest")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fortytw2/leaktest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0487zghyxqzk6zdbhd2j074pcc2l15l4sfg5clrjqwfbql7519wx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/fortytw2/leaktest"))
    (home-page "https://github.com/fortytw2/leaktest")
    (synopsis "Leaktest")
    (description
     "Package leaktest provides tools to detect leaked goroutines in tests.  To use
it, call \"defer @code{leaktest.Check(t)()}\" at the beginning of each test that
may use goroutines.  copied out of the cockroachdb source tree with slight
modifications to be more re-useable.")
    (license license:bsd-3)))

(define-public go-github-com-foxcpp-go-mockdns
  (package
    (name "go-github-com-foxcpp-go-mockdns")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/foxcpp/go-mockdns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b88ra4y1yn3wzlwn2ajg11vg44pz6h741ac7gai7gmq12lmk18m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/foxcpp/go-mockdns"))
    (propagated-inputs (list go-github-com-miekg-dns))
    (home-page "https://github.com/foxcpp/go-mockdns")
    (synopsis "go-mockdns")
    (description
     "Boilerplate for testing of code involving DNS lookups, including hacks to
redirect @@code{net.Lookup*} calls.")
    (license license:expat)))

(define-public go-github-com-tchap-go-patricia
  (package
    (name "go-github-com-tchap-go-patricia")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tchap/go-patricia")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05jri24s81bswsqp7idniz41qba52131g156b5br5z4ignqx8s5x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tchap/go-patricia/v2"
      #:unpack-path "github.com/tchap/go-patricia"))
    (home-page "https://github.com/tchap/go-patricia")
    (synopsis "go-patricia")
    (description
     "This package provides a generic patricia trie (also called radix tree)
implemented in Go (Golang).")
    (license license:expat)))

(define-public go-github-com-yashtewari-glob-intersection
  (package
    (name "go-github-com-yashtewari-glob-intersection")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yashtewari/glob-intersection")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sw3dag2qwx14z0pj1l0v33majk82s1vx9fsnjr3l69dq7jf212b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yashtewari/glob-intersection"))
    (home-page "https://github.com/yashtewari/glob-intersection")
    (synopsis "glob-intersection")
    (description
     "Package gintersect provides methods to check whether the intersection of several
globs matches a non-empty set of strings.")
    (license license:asl2.0)))

(define-public go-oras-land-oras-go
  (package
    (name "go-oras-land-oras-go")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oras-project/oras-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y2cdpy13gfmjfdpqlcq4ss36avzvfjmmfadc82bawg8jwi0j2jk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "oras.land/oras-go/v2"
      #:unpack-path "oras.land/oras-go"))
    (propagated-inputs (list go-golang-org-x-sync
                             go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-go-digest))
    (home-page "https://oras.land/oras-go")
    (synopsis "ORAS Go library")
    (description
     "@@code{oras-go} is a Go library for managing OCI artifacts, compliant with the
@@url{https://github.com/opencontainers/image-spec,OCI Image Format
Specification} and the
@@url{https://github.com/opencontainers/distribution-spec,OCI Distribution
Specification}.  It provides unified APIs for pushing, pulling, and managing
artifacts across OCI-compliant registries, local file systems, and in-memory
stores.")
    (license license:asl2.0)))

(define-public go-github-com-open-policy-agent-opa
  (package
    (name "go-github-com-open-policy-agent-opa")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-policy-agent/opa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vsfxyrf0qi5fzpgy8fssm1abpxy017lvqlh5cai2l18gzxj5340"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/open-policy-agent/opa"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                        go-oras-land-oras-go-v2
                        go-gopkg-in-yaml-v3
                        go-gopkg-in-check-v1
                        go-google-golang-org-grpc
                        go-golang-org-x-time
                        go-golang-org-x-net
                        go-go-uber-org-automaxprocs
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-github-com-yashtewari-glob-intersection
                        go-github-com-xeipuuv-gojsonreference
                        go-github-com-vektah-gqlparser-v2
                        go-github-com-tchap-go-patricia-v2
                        go-github-com-spf13-viper
                        go-github-com-spf13-pflag
                        go-github-com-spf13-cobra
                        go-github-com-sirupsen-logrus
                        go-github-com-sergi-go-diff
                        go-github-com-rcrowley-go-metrics
                        go-github-com-prometheus-client-model
                        go-github-com-prometheus-client-golang
                        go-github-com-peterh-liner
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-olekukonko-tablewriter
                        go-github-com-gorilla-mux
                        go-github-com-google-uuid
                        go-github-com-google-go-cmp
                        go-github-com-golang-protobuf
                        go-github-com-gobwas-glob
                        go-github-com-go-logr-logr
                        go-github-com-go-ini-ini
                        go-github-com-fsnotify-fsnotify
                        go-github-com-foxcpp-go-mockdns
                        go-github-com-fortytw2-leaktest
                        go-github-com-dgraph-io-badger-v4
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-containerd-v2
                        go-github-com-cespare-xxhash-v2
                        go-github-com-bytecodealliance-wasmtime-go-v3))
    (home-page "https://github.com/open-policy-agent/opa")
    (synopsis "Open Policy Agent")
    (description
     "Open Policy Agent (OPA) is an open source, general-purpose policy engine that
enables unified, context-aware policy enforcement across the entire stack.")
    (license license:asl2.0)))

(define-public go-github-com-checkpoint-restore-go-criu
  (package
    (name "go-github-com-checkpoint-restore-go-criu")
    (version "6.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/checkpoint-restore/go-criu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b7427rqf1il6pjbgzdm8vwcxvcf013d5sa13k7fi8pmifqb81dz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/checkpoint-restore/go-criu/v6"
      #:unpack-path "github.com/checkpoint-restore/go-criu"))
    (propagated-inputs (list go-google-golang-org-protobuf go-golang-org-x-sys
                             go-github-com-spf13-cobra))
    (home-page "https://github.com/checkpoint-restore/go-criu")
    (synopsis "go-criu -- Go bindings for CRIU")
    (description
     "This repository provides Go bindings for @@url{https://criu.org/,CRIU}.  The
code is based on the Go-based PHaul implementation from the CRIU repository.
For easier inclusion into other Go projects, the CRIU Go bindings have been
moved to this repository.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-capability
  (package
    (name "go-github-com-moby-sys-capability")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "capability"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1140wqx5mlr9adk74k6bsswqm6dhps02cwv6k8j6nssd7ln3v514"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/capability"
      #:unpack-path "github.com/moby/sys"))
    (home-page "https://github.com/moby/sys")
    (synopsis "Alternatives")
    (description
     "Package capability provides utilities for manipulating POSIX capabilities.")
    (license license:bsd-2)))

(define-public go-github-com-mrunalp-fileutils
  (package
    (name "go-github-com-mrunalp-fileutils")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrunalp/fileutils")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nanl3vypvjlmjyjmv1ngz3lxvc5l55cn9xgr6k36wck5l37jcpi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mrunalp/fileutils"))
    (home-page "https://github.com/mrunalp/fileutils")
    (synopsis "fileutils")
    (description "Collection of utilities for file manipulation in golang.")
    (license license:asl2.0)))

(define-public go-github-com-opencontainers-cgroups
  (package
    (name "go-github-com-opencontainers-cgroups")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/cgroups")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b1h0100244jkramd0clnsfdf7az20v2r43apls8s60irz8yxn3i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/opencontainers/cgroups"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-sirupsen-logrus
                             go-github-com-moby-sys-userns
                             go-github-com-moby-sys-mountinfo
                             go-github-com-godbus-dbus-v5
                             go-github-com-cyphar-filepath-securejoin
                             go-github-com-coreos-go-systemd-v22
                             go-github-com-cilium-ebpf))
    (home-page "https://github.com/opencontainers/cgroups")
    (synopsis "OCI Project Template")
    (description
     "Useful boilerplate and organizational information for all OCI projects.")
    (license license:asl2.0)))

(define-public go-github-com-seccomp-libseccomp-golang
  (package
    (name "go-github-com-seccomp-libseccomp-golang")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/seccomp/libseccomp-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lbrs4j8b8w47awczydb8snrky6gjgdfmcsvlwx4wvq20ipqr1nj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/seccomp/libseccomp-golang"))
    (home-page "https://github.com/seccomp/libseccomp-golang")
    (synopsis #f)
    (description
     "Package seccomp provides bindings for libseccomp, a library wrapping the Linux
seccomp syscall.  Seccomp enables an application to restrict system call use for
itself and its children.")
    (license license:bsd-2)))

(define-public go-github-com-opencontainers-runc
  (package
    (name "go-github-com-opencontainers-runc")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/runc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0midvxwmj4fvhy5mqv616bhlx39j0gd6y890adx7dnz5in506ym1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/opencontainers/runc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-golang-org-x-sys
                             go-golang-org-x-net
                             go-github-com-vishvananda-netlink
                             go-github-com-urfave-cli
                             go-github-com-sirupsen-logrus
                             go-github-com-seccomp-libseccomp-golang
                             go-github-com-opencontainers-selinux
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-opencontainers-cgroups
                             go-github-com-mrunalp-fileutils
                             go-github-com-moby-sys-userns
                             go-github-com-moby-sys-user
                             go-github-com-moby-sys-mountinfo
                             go-github-com-moby-sys-capability
                             go-github-com-godbus-dbus-v5
                             go-github-com-docker-go-units
                             go-github-com-cyphar-filepath-securejoin
                             go-github-com-coreos-go-systemd-v22
                             go-github-com-containerd-console
                             go-github-com-checkpoint-restore-go-criu-v6))
    (home-page "https://github.com/opencontainers/runc")
    (synopsis "runc")
    (description
     "@@code{runc} is a CLI tool for spawning and running containers on Linux
according to the OCI specification.")
    (license license:asl2.0)))

(define-public go-github-com-microsoft-hcsshim
  (package
    (name "go-github-com-microsoft-hcsshim")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/hcsshim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r1dapf81zsc59ma0ik6h8ajf6b21llanaa1kljnf16iqn4v52gw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Microsoft/hcsshim"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc-cmd-protoc-gen-go-grpc
                             go-google-golang-org-grpc
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-go-uber-org-mock
                             go-go-opencensus-io
                             go-go-etcd-io-bbolt
                             go-github-com-vishvananda-netns
                             go-github-com-vishvananda-netlink
                             go-github-com-urfave-cli
                             go-github-com-sirupsen-logrus
                             go-github-com-pkg-errors
                             go-github-com-pelletier-go-toml
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-opencontainers-runc
                             go-github-com-open-policy-agent-opa
                             go-github-com-moby-sys-user
                             go-github-com-mattn-go-shellwords
                             go-github-com-linuxkit-virtsock
                             go-github-com-josephspurrier-goversioninfo
                             go-github-com-google-go-containerregistry
                             go-github-com-google-go-cmp
                             go-github-com-containerd-typeurl-v2
                             go-github-com-containerd-ttrpc
                             go-github-com-containerd-protobuild
                             go-github-com-containerd-go-runc
                             go-github-com-containerd-errdefs-pkg
                             go-github-com-containerd-errdefs
                             go-github-com-containerd-containerd-api
                             go-github-com-containerd-containerd
                             go-github-com-containerd-console
                             go-github-com-containerd-cgroups-v3
                             go-github-com-cenkalti-backoff-v4
                             go-github-com-blang-semver-v4
                             go-github-com-microsoft-go-winio
                             go-github-com-microsoft-didx509go
                             go-github-com-microsoft-cosesign1go))
    (home-page "https://github.com/Microsoft/hcsshim")
    (synopsis "hcsshim")
    (description
     "This package contains the Golang interface for using the Windows
@@url{https://techcommunity.microsoft.com/t5/containers/introducing-the-host-compute-service-hcs/ba-p/382332,Host
Compute Service} (HCS) to launch and manage
@@url{https://docs.microsoft.com/en-us/virtualization/windowscontainers/about/,Windows
Containers}.  It also contains other helpers and functions for managing Windows
Containers such as the Golang interface for the Host Network Service (HNS), as
well as code for the
@@url{https://github.com/Microsoft/hcsshim/blob/v0.13.0/internal/guest/README.md,guest
agent} (commonly referred to as the GCS or Guest Compute Service in the
codebase) used to support running Linux Hyper-V containers.")
    (license license:expat)))

(define-public go-github-com-alexflint-go-filemutex
  (package
    (name "go-github-com-alexflint-go-filemutex")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexflint/go-filemutex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kx31qar8265jrvdvwja1snlfy6kxpaay654lqqsalp1spgrcazp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alexflint/go-filemutex"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/alexflint/go-filemutex")
    (synopsis "FileMutex")
    (description
     "@code{FileMutex} is similar to @@code{sync.RWMutex}, but also synchronizes
across processes.  On Linux, OSX, and other POSIX systems it uses the flock
system call.  On windows it uses the @code{LockFileEx} and @code{UnlockFileEx}
system calls.")
    (license license:expat)))

(define-public go-github-com-buger-jsonparser
  (package
    (name "go-github-com-buger-jsonparser")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buger/jsonparser")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qv2lsh2biwxn927941gqiv5pqg7n4v58j0i536pjp7pr17pq7dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/buger/jsonparser"))
    (home-page "https://github.com/buger/jsonparser")
    (synopsis
     "Alternative JSON parser for Go (10x times faster standard library)")
    (description
     "It does not require you to know the structure of the payload (eg.  create
structs), and allows accessing fields by providing the path to them.  It is up
to @@strong{10 times faster} than standard @@code{encoding/json} package
(depending on payload size and usage), @@strong{allocates no memory}.  See
benchmarks below.")
    (license license:expat)))

(define-public go-github-com-containernetworking-cni
  (package
    (name "go-github-com-containernetworking-cni")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containernetworking/cni")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x4apykvfwbx282hgrc9151rb1kx9w40kzfv78x548hrryqa7rn5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/containernetworking/cni"))
    (propagated-inputs (list go-github-com-vishvananda-netns
                             go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo-v2))
    (home-page "https://github.com/containernetworking/cni")
    (synopsis "CNI - the Container Network Interface")
    (description
     "CNI (), a @@url{https://cncf.io,Cloud Native Computing Foundation} project,
consists of a specification and libraries for writing plugins to configure
network interfaces in Linux containers, along with a number of supported
plugins.  CNI concerns itself only with network connectivity of containers and
removing allocated resources when the container is deleted.  Because of this
focus, CNI has a wide range of support and the specification is simple to
implement.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-iptables
  (package
    (name "go-github-com-coreos-go-iptables")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coreos/go-iptables")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xxzqz9np93d8iig5dwjjpb78pqdj74zr91qb11r7g30nkcak5sw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/coreos/go-iptables"))
    (home-page "https://github.com/coreos/go-iptables")
    (synopsis "go-iptables")
    (description "Go bindings for iptables utility.")
    (license license:asl2.0)))

(define-public go-github-com-hugelgupf-socketpair
  (package
    (name "go-github-com-hugelgupf-socketpair")
    (version "0.0.0-20240723164047-9246f217b3fc")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hugelgupf/socketpair")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pdaanxirsnw9d63mz3867q8vgfh51099zzm4mm6ngv8a14zxzcl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hugelgupf/socketpair"))
    (home-page "https://github.com/hugelgupf/socketpair")
    (synopsis "socketpair")
    (description
     "Package socketpair provides bidirectionally connected net.Conns.")
    (license license:bsd-3)))

(define-public go-github-com-mdlayher-packet
  (package
    (name "go-github-com-mdlayher-packet")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/packet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17jms60j1xygsn5z61r2kz8yywsihf0xjzlhq3v5cpqf08mljwsn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mdlayher/packet"))
    (propagated-inputs (list go-golang-org-x-sys go-golang-org-x-net
                             go-github-com-mdlayher-socket
                             go-github-com-josharian-native
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/mdlayher/packet")
    (synopsis "packet")
    (description
     "Package packet provides access to Linux packet sockets (AF_PACKET).")
    (license license:expat)))

(define-public go-github-com-u-root-uio
  (package
    (name "go-github-com-u-root-uio")
    (version "0.0.0-20240224005618-d2acac8f3701")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/u-root/uio")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ckxps3xbgllij3wnmmp2k9ip3z9v8vyc4wdcv43fxczqfiq25xz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/u-root/uio"))
    (propagated-inputs (list go-golang-org-x-sys go-github-com-pierrec-lz4-v4))
    (home-page "https://github.com/u-root/uio")
    (synopsis "uio")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-insomniacslk-dhcp
  (package
    (name "go-github-com-insomniacslk-dhcp")
    (version "0.0.0-20250417080101-5f8cf70e8c5f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/insomniacslk/dhcp")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1plxb84g7mn1iabgjjw71n16vc17m2kay3snihkydgb628j58hqv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/insomniacslk/dhcp"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-net
                             go-github-com-u-root-uio
                             go-github-com-stretchr-testify
                             go-github-com-mdlayher-packet
                             go-github-com-mdlayher-netlink
                             go-github-com-jsimonetti-rtnetlink
                             go-github-com-hugelgupf-socketpair
                             go-github-com-google-go-cmp))
    (home-page "https://github.com/insomniacslk/dhcp")
    (synopsis "dhcp")
    (description
     "DHCPv4 and DHCPv6 decoding/encoding library with client and server code, written
in Go.")
    (license license:bsd-3)))

(define-public go-github-com-networkplumbing-go-nft
  (package
    (name "go-github-com-networkplumbing-go-nft")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/networkplumbing/go-nft")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gx0xh9llgi600v6qacnaxwk3j0kmmwk7d2hm9j5jpmcm8whvp2w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/networkplumbing/go-nft"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/networkplumbing/go-nft")
    (synopsis "go-nft")
    (description "Go bindings for nft utility.")
    (license license:asl2.0)))

(define-public go-sigs-k8s-io-knftables
  (package
    (name "go-sigs-k8s-io-knftables")
    (version "0.0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kubernetes-sigs/knftables")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03i92dlsyc2y50nqfznfg54mn976rz4fc1a9pnaadqcy5zi00ak4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "sigs.k8s.io/knftables"))
    (propagated-inputs (list go-github-com-lithammer-dedent
                             go-github-com-google-go-cmp))
    (home-page "https://sigs.k8s.io/knftables")
    (synopsis "knftables: a golang nftables library")
    (description "This is a library for using nftables from Go.")
    (license license:asl2.0)))

(define-public go-github-com-containernetworking-plugins
  (package
    (name "go-github-com-containernetworking-plugins")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/containernetworking/plugins")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04acfjkc3z2yf85z0v1mlgj0fqy032dsklzik2g1cnz6ncw6jl2b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/containernetworking/plugins"))
    (propagated-inputs (list go-sigs-k8s-io-knftables
                             go-golang-org-x-sys
                             go-github-com-vishvananda-netns
                             go-github-com-vishvananda-netlink
                             go-github-com-safchain-ethtool
                             go-github-com-pkg-errors
                             go-github-com-opencontainers-selinux
                             go-github-com-onsi-gomega
                             go-github-com-onsi-ginkgo-v2
                             go-github-com-networkplumbing-go-nft
                             go-github-com-mattn-go-shellwords
                             go-github-com-insomniacslk-dhcp
                             go-github-com-godbus-dbus-v5
                             go-github-com-coreos-go-systemd-v22
                             go-github-com-coreos-go-iptables
                             go-github-com-containernetworking-cni
                             go-github-com-buger-jsonparser
                             go-github-com-alexflint-go-filemutex
                             go-github-com-microsoft-hcsshim))
    (home-page "https://github.com/containernetworking/plugins")
    (synopsis "Plugins")
    (description
     "Some CNI network plugins, maintained by the containernetworking team.  For more
information, see the @@url{https://www.cni.dev,CNI website}.")
    (license license:asl2.0)))

(define-public go-github-com-distribution-reference
  (package
    (name "go-github-com-distribution-reference")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/distribution/reference")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zj2lmmznlrxdrrfmdsx7fgrmi64bj1jqz6r0ar35qmkx8pjvgl2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/distribution/reference"))
    (propagated-inputs (list go-github-com-opencontainers-go-digest))
    (home-page "https://github.com/distribution/reference")
    (synopsis "Distribution reference")
    (description
     "Package reference provides a general type to represent any way of referencing
images within the registry.  Its main purpose is to abstract tags and digests
(content-addressable hash).")
    (license license:asl2.0)))

(define-public go-github-com-docker-docker
  (package
    (name "go-github-com-docker-docker")
    (version "28.2.2+incompatible")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/moby")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w859a1q82aw58a2kb4z124igxzg015dipi1vqyb4bj9s3c8yv33"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/docker/docker"))
    (home-page "https://github.com/docker/docker")
    (synopsis "The Moby Project")
    (description
     "Moby is an open-source project created by Docker to enable and accelerate
software containerization.")
    (license license:asl2.0)))

(define-public go-github-com-codahale-rfc6979
  (package
    (name "go-github-com-codahale-rfc6979")
    (version "0.0.0-20141003034818-6a90f24967eb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/codahale/rfc6979")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07p51hmvvzvn84mg1c3kmp799j7jf7w15770qw8qz9q7j8frx03f"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/codahale/rfc6979"))
    (home-page "https://github.com/codahale/rfc6979")
    (synopsis "rfc6979")
    (description
     "Package rfc6979 is an implementation of
@@url{https://rfc-editor.org/rfc/rfc6979.html,RFC 6979}'s deterministic DSA.")
    (license license:expat)))

(define-public go-github-com-secure-systems-lab-go-securesystemslib
  (package
    (name "go-github-com-secure-systems-lab-go-securesystemslib")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/secure-systems-lab/go-securesystemslib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ifi33vqy7dkb6jf8h9vilbl9yc0v2gz8nqh73rjc9adsm0z4n2r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/secure-systems-lab/go-securesystemslib"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-codahale-rfc6979))
    (home-page "https://github.com/secure-systems-lab/go-securesystemslib")
    (synopsis #f)
    (description
     "This package provides a library that provides cryptographic and general-purpose
functions for Go Secure Systems Lab projects at NYU.")
    (license license:expat)))

(define-public go-github-com-shibumi-go-pathspec
  (package
    (name "go-github-com-shibumi-go-pathspec")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shibumi/go-pathspec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wsnn0di87qdp171j8x27qzf9f59hdqc50jna9mj5iryzavxywk4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shibumi/go-pathspec"))
    (home-page "https://github.com/shibumi/go-pathspec")
    (synopsis "go-pathspec")
    (description
     "Package pathspec implements git compatible gitignore pattern matching.  See the
description below, if you are unfamiliar with it:.")
    (license license:asl2.0)))

(define-public go-github-com-zeebo-errs
  (package
    (name "go-github-com-zeebo-errs")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/errs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bvmqpxr7gk38yl1wz6llwjgd5ys3ls0m150wjmmr61nggb5n7dy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/errs"))
    (home-page "https://github.com/zeebo/errs")
    (synopsis "errs")
    (description
     "Package errs provides a simple error package with stack traces.")
    (license license:expat)))

(define-public go-cel-dev-expr
  (package
    (name "go-cel-dev-expr")
    (version "0.24.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/cel-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fzy5njwzg48h1mqbfhczyq6hxmbq3yzdivkjh1x8ipj19v4hvfl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "cel.dev/expr"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-genproto-googleapis-rpc))
    (home-page "https://cel.dev/expr")
    (synopsis "Common Expression Language")
    (description
     "The Common Expression Language (CEL) implements common semantics for expression
evaluation, enabling different applications to more easily interoperate.")
    (license license:asl2.0)))

(define-public go-github-com-cncf-xds-go
  (package
    (name "go-github-com-cncf-xds-go")
    (version "0.0.0-20250501225837-2ac532fd4443")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cncf/xds")
             (commit (go-version->git-ref version
                                          #:subdir "go"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0snccl7rrx2f07131s0n8z3d1p5v2nvcg7v3dhq1a5sl46b1x78b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cncf/xds/go"
      #:unpack-path "github.com/cncf/xds"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-api
                             go-github-com-envoyproxy-protoc-gen-validate
                             go-cel-dev-expr))
    (home-page "https://github.com/cncf/xds")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
  (package
    (name "go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "exporters/stdout/stdouttrace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/otel/exporters/stdout/stdouttrace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "STDOUT Trace Exporter")
    (description
     "Package stdouttrace contains an @code{OpenTelemetry} exporter for tracing
telemetry to be written to an output destination as JSON.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-security-advancedtls
  (package
    (name "go-google-golang-org-grpc-security-advancedtls")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (go-version->git-ref version
                                          #:subdir "security/advancedtls"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xkqjian41falr0h8sicx2vdajf1zxcrkqiz5p2g7mmm8gcb6l4w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/grpc/security/advancedtls"
      #:unpack-path "google.golang.org/grpc"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-google-go-cmp))
    (home-page "https://google.golang.org/grpc")
    (synopsis #f)
    (description
     "Package advancedtls provides @code{gRPC} transport credentials that allow easy
configuration of advanced TLS features.  The APIs here give the user more
customizable control to fit their security landscape, thus the \"advanced\"
moniker.  This package provides both interfaces and generally useful
implementations of those interfaces, for example periodic credential reloading,
support for certificate revocation lists, and customizable certificate
verification behaviors.  If the provided implementations do not fit a given use
case, a custom implementation of the interface can be injected.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-examples
  (package
    (name "go-google-golang-org-grpc-examples")
    (version "0.0.0-20250611162622-57400b4e6918")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (go-version->git-ref version
                                          #:subdir "examples"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zvhljx6p1n780msnypz81b62gzwd5ssy77gbkyblf9yg8n58qsa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/grpc/examples"
      #:unpack-path "google.golang.org/grpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                        go-google-golang-org-grpc-security-advancedtls
                        go-google-golang-org-genproto-googleapis-rpc
                        go-golang-org-x-oauth2
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
                        go-go-opentelemetry-io-otel-exporters-prometheus
                        go-go-opentelemetry-io-otel
                        go-github-com-prometheus-client-golang
                        go-github-com-cncf-xds-go))
    (home-page "https://google.golang.org/grpc")
    (synopsis "Examples")
    (description
     "The following examples are provided to help users get started with
@code{gRPC-Go}.  They are arranged as follows:.")
    (license license:asl2.0)))

(define-public go-github-com-spiffe-go-spiffe
  (package
    (name "go-github-com-spiffe-go-spiffe")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spiffe/go-spiffe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "076fx3xbazk3r3h8a5pc0z40i0d7hxiqccc7lzi317k87kf284rq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/spiffe/go-spiffe/v2"
      #:unpack-path "github.com/spiffe/go-spiffe"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc-examples
                             go-google-golang-org-grpc
                             go-github-com-zeebo-errs
                             go-github-com-stretchr-testify
                             go-github-com-go-jose-go-jose-v4
                             go-github-com-microsoft-go-winio))
    (home-page "https://github.com/spiffe/go-spiffe")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-in-toto-in-toto-golang
  (package
    (name "go-github-com-in-toto-in-toto-golang")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/in-toto/in-toto-golang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ravkfalyzwd57x7hyc243x8dc7g2cb9xjka1hprpnfz2m33qpy0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/in-toto/in-toto-golang"))
    (propagated-inputs (list go-google-golang-org-grpc
                        go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-spiffe-go-spiffe-v2
                        go-github-com-spf13-cobra
                        go-github-com-shibumi-go-pathspec
                        go-github-com-secure-systems-lab-go-securesystemslib
                        go-github-com-google-go-cmp))
    (home-page "https://github.com/in-toto/in-toto-golang")
    (synopsis "Go implementation of in-toto")
    (description
     "Go implementation of the
@@url{https://github.com/in-toto/docs/blob/master/in-toto-spec.md,in-toto
specification}.")
    (license license:asl2.0)))

(define-public go-github-com-mitchellh-hashstructure
  (package
    (name "go-github-com-mitchellh-hashstructure")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitchellh/hashstructure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yyr1igvyv7dzjxs9hbwk7qhshwxys0hq59sy2g2a46hjgi311iv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mitchellh/hashstructure/v2"
      #:unpack-path "github.com/mitchellh/hashstructure"))
    (home-page "https://github.com/mitchellh/hashstructure")
    (synopsis "hashstructure")
    (description
     "hashstructure is a Go library for creating a unique hash value for arbitrary
values in Go.")
    (license license:expat)))

(define-public go-github-com-moby-docker-image-spec
  (package
    (name "go-github-com-moby-docker-image-spec")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/docker-image-spec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06r6z8s0rvl66n626q41hmqgnnlpsqdblj32fjq3r0qsccp8s167"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/docker-image-spec"))
    (propagated-inputs (list go-github-com-opencontainers-image-spec))
    (home-page "https://github.com/moby/docker-image-spec")
    (synopsis "Docker Image Specification v1.")
    (description
     "This directory contains documents about Docker Image Specification v1.X.")
    (license license:asl2.0)))

(define-public go-github-com-adalogics-go-fuzz-headers
  (package
    (name "go-github-com-adalogics-go-fuzz-headers")
    (version "0.0.0-20240806141605-e8a1dd7889d6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AdaLogics/go-fuzz-headers")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "161wky8n1zszn34zgh837lpk6q3cabfhzavv1qyzd0qybmq1n7g2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/AdaLogics/go-fuzz-headers"))
    (home-page "https://github.com/AdaLogics/go-fuzz-headers")
    (synopsis "go-fuzz-headers")
    (description
     "This repository contains various helper functions for go fuzzing.  It is mostly
used in combination with @@url{https://github.com/dvyukov/go-fuzz,go-fuzz}, but
compatibility with fuzzing in the standard library will also be supported.  Any
coverage guided fuzzing engine that provides an array or slice of bytes can be
used with go-fuzz-headers.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-mount
  (package
    (name "go-github-com-moby-sys-mount")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "mount"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nh1disclgydvq7k10awzks6k8kw9cjj3q19f83ksi4b76p5l475"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/mount"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-github-com-moby-sys-mountinfo))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description
     "Package mount provides a set of functions to mount and unmount mounts.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-sequential
  (package
    (name "go-github-com-moby-sys-sequential")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "sequential"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/sequential"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description
     "Package sequential provides a set of functions for managing sequential files on
Windows.")
    (license license:asl2.0)))

(define-public go-github-com-moby-go-archive
  (package
    (name "go-github-com-moby-go-archive")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/go-archive")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyz6nxbs368wf4ndn0l6b190rb303frf1sbp5c5s09law14cs24"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/moby/go-archive"))
    (propagated-inputs (list go-gotest-tools-v3
                             go-golang-org-x-sys
                             go-github-com-moby-sys-userns
                             go-github-com-moby-sys-user
                             go-github-com-moby-sys-sequential
                             go-github-com-moby-sys-reexec
                             go-github-com-moby-sys-mountinfo
                             go-github-com-moby-sys-mount
                             go-github-com-moby-patternmatcher
                             go-github-com-klauspost-compress
                             go-github-com-google-go-cmp
                             go-github-com-containerd-log
                             go-github-com-adalogics-go-fuzz-headers))
    (home-page "https://github.com/moby/go-archive")
    (synopsis #f)
    (description
     "Package archive provides helper functions for dealing with archive files.")
    (license license:asl2.0)))

(define-public go-github-com-moby-locker
  (package
    (name "go-github-com-moby-locker")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/locker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07rc2c6h35f9mcy81jp382a030f6xmcifi9n5jnlayybfwxmpjir"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/locker"))
    (home-page "https://github.com/moby/locker")
    (synopsis "Locker")
    (description
     "Package locker provides a mechanism for creating finer-grained locking to help
free up more global locks to handle other tasks.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-reexec
  (package
    (name "go-github-com-moby-sys-reexec")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "reexec"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n2z0zqfdyw6rllqdljddczh758kq22k4ajrhv27shv7m3fnvm0p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/reexec"
      #:unpack-path "github.com/moby/sys"))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description
     "Package reexec facilitates the busybox style reexec of a binary.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-signal
  (package
    (name "go-github-com-moby-sys-signal")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "signal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i1phx1kk9qa4jf1i1nl23d3f6k9fn2w46274cl76cqw9hjqg868"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/signal"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description
     "Package signal provides helper functions for dealing with signals across various
operating systems.")
    (license license:asl2.0)))

(define-public go-github-com-moby-sys-user
  (package
    (name "go-github-com-moby-sys-user")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/sys")
             (commit (go-version->git-ref version
                                          #:subdir "user"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ayv2f12za923fzyf6j4d39l54xwaijbq0xfrlfdb8xsif4nlfnb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/sys/user"
      #:unpack-path "github.com/moby/sys"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/moby/sys")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-morikuni-aec
  (package
    (name "go-github-com-morikuni-aec")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/morikuni/aec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qaqh0lk9wrqgff0yrxnbznvmwyhdxy3g9b2hjpazp5bw4nj0dp7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/morikuni/aec"))
    (home-page "https://github.com/morikuni/aec")
    (synopsis "aec")
    (description "Go wrapper for ANSI escape code.")
    (license license:expat)))

(define-public go-github-com-package-url-packageurl-go
  (package
    (name "go-github-com-package-url-packageurl-go")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/package-url/packageurl-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i8zgznr56kr9jn0kyc1w39dbj93d589rk44hrd5qx2750wzs890"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/package-url/packageurl-go"))
    (home-page "https://github.com/package-url/packageurl-go")
    (synopsis "packageurl-go")
    (description "Package packageurl implements the package-url spec.")
    (license license:expat)))

(define-public go-github-com-serialx-hashring
  (package
    (name "go-github-com-serialx-hashring")
    (version "0.0.0-20200727003509-22c0c7ab6b1b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/serialx/hashring")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbqc69z6hjqvvbbls97cizb3rixil67jq37fnjw2pkf0zhs9i4q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/serialx/hashring"))
    (home-page "https://github.com/serialx/hashring")
    (synopsis "hashring")
    (description
     "This package implements consistent hashing that can be used when the number of
server nodes can increase or decrease (like in memcached).  The hashing ring is
built using the same algorithm as libketama.")
    (license license:expat)))

(define-public go-github-com-anchore-go-struct-converter
  (package
    (name "go-github-com-anchore-go-struct-converter")
    (version "0.0.0-20250211213226-cce56d595160")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anchore/go-struct-converter")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h5gjk3znagv882jqbyfz244033h4x7g2xz66aj3vskh37jgjrfc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/anchore/go-struct-converter"))
    (home-page "https://github.com/anchore/go-struct-converter")
    (synopsis "Go")
    (description
     "This package provides a library for converting between Go structs.")
    (license license:asl2.0)))

(define-public go-github-com-spdx-gordf
  (package
    (name "go-github-com-spdx-gordf")
    (version "0.0.0-20250128162952-000978ccd6fb")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spdx/gordf")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0almcq8p3sj0wbp3c276xdhva1imwnprvbd7klgfjnzlh11pllcw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spdx/gordf"))
    (home-page "https://github.com/spdx/gordf")
    (synopsis "gordf")
    (description
     "gordf is a package which provides a parser for RDF files linearized using
RDF/XML format.  It will be used to represent the rdf files in memory and write
back in possibly different formats like json, and xml.")
    (license license:expat)))

(define-public go-github-com-microsoft-go-winio
  (package
    (name "go-github-com-microsoft-go-winio")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/go-winio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09dxp536m15d3l9aibpfgs9ag55n7gqrgp5app4rcb888c6mclxm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Microsoft/go-winio"))
    (propagated-inputs (list go-golang-org-x-tools go-golang-org-x-sys
                             go-github-com-sirupsen-logrus))
    (home-page "https://github.com/Microsoft/go-winio")
    (synopsis "go-winio")
    (description
     "This package provides utilities for efficiently performing Win32 IO operations
in Go.  Currently, this package is provides support for genreal IO and
management of.")
    (license license:expat)))

(define-public go-github-com-planetscale-vtprotobuf
  (package
    (name "go-github-com-planetscale-vtprotobuf")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/planetscale/vtprotobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bms8rrg8wrm3x9mspqrzzix24vjgi3p5zzbw108ydr1rnarwblf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/planetscale/vtprotobuf"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/planetscale/vtprotobuf")
    (synopsis ", the Vitess Protocol Buffers compiler")
    (description
     "This repository provides the @@code{protoc-gen-go-vtproto} plug-in for
@@code{protoc}, which is used by Vitess to generate optimized marshall &
unmarshal code.")
    (license license:bsd-3)))

(define-public go-github-com-tonistiigi-dchapes-mode
  (package
    (name "go-github-com-tonistiigi-dchapes-mode")
    (version "0.0.0-20250318174251-73d941a28323")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/dchapes-mode")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bs5jvq3045mspyq42pk3dliqgn3a975l30030abvggidnbxpggw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/dchapes-mode"))
    (home-page "https://github.com/tonistiigi/dchapes-mode")
    (synopsis "Mode")
    (description
     "Package mode provides a native Go implementation of BSD's setmode and getmode
which can be used to modify the mode bits of an os.@code{FileMode} value based
on a symbolic value as described by the Unix chmod command.")
    (license license:bsd-2)))

(define-public go-github-com-tonistiigi-fsutil
  (package
    (name "go-github-com-tonistiigi-fsutil")
    (version "0.0.0-20250605211040-586307ad452f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/fsutil")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ydaqjk6lvgwpbymkf9y6nm9syxg7hs3k0h00xk48vpb85k6rn4q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/fsutil"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-tonistiigi-dchapes-mode
                             go-github-com-stretchr-testify
                             go-github-com-planetscale-vtprotobuf
                             go-github-com-pkg-errors
                             go-github-com-opencontainers-go-digest
                             go-github-com-moby-patternmatcher
                             go-github-com-containerd-continuity
                             go-github-com-microsoft-go-winio))
    (home-page "https://github.com/tonistiigi/fsutil")
    (synopsis #f)
    (description "Incremental file directory sync tools in golang.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v2
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/resourcemanager/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0db2hxq8wigvmqkj8h5gfq9lw6bp0k13hvcyi0hxgnriz1mdvpx5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/internal/v2"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        ;; go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v3
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/resourcemanager/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q961bbvzkr1ya7avy5sr3gp8jbzngdlry7i8jmvgry8x6gk3x4z"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/internal/v3"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azcore
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azcore")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azcore"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "135h8qdb2yjgd51xbkk50d0nh0w2pmi6v35dkjzdk73ci30kl8ky"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azcore"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-internal))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Core Client Module for Go")
    (description
     "Package azcore implements an HTTP request/response middleware pipeline used by
Azure SDK clients.")
    (license license:expat)))

(define-public go-github-com-keybase-go-keychain
  (package
    (name "go-github-com-keybase-go-keychain")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/keybase/go-keychain")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gkd839h8xnfiv0g52hm4p9snrcfgrnczrqf5wxr61sgg2w8h3y1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/keybase/go-keychain"))
    (propagated-inputs (list go-golang-org-x-crypto
                             go-github-com-stretchr-testify
                             go-github-com-keybase-dbus))
    (home-page "https://github.com/keybase/go-keychain")
    (synopsis "Go Keychain")
    (description
     "This package provides a library for accessing the Keychain for @code{macOS},
@code{iOS}, and Linux in Go (golang).")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
  (package
    (name
     "go-github-com-azuread-microsoft-authentication-extensions-for-go-cache")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jj7bjdns1dsxsk22p0xdph3j9hhysbwcbxwsqpxyyggzni9zijv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go/cache"
      #:unpack-path
      "github.com/AzureAD/microsoft-authentication-extensions-for-go"))
    (propagated-inputs (list go-gopkg-in-check-v1 go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-keybase-go-keychain
                        go-github-com-azuread-microsoft-authentication-library-for-go))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-extensions-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) Extensions for Go")
    (description
     "This module contains a persistent cache for
@@url{https://github.com/@code{AzureAD/microsoft-authentication-library-for-go,Microsoft}
Authentication Library (MSAL) for Go} public client applications such as CLI
tools.  It isn't recommended for web applications or RPC APIs, in which it can
cause scaling and performance problems.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity/cache"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x4bb3bgy4pgz67nwjjxiil4q9cmnjhnl8zh7vcrxirbwprpchja"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity/cache"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-sys
                        go-github-com-stretchr-testify
                        go-github-com-google-uuid
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azuread-microsoft-authentication-extensions-for-go-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Cache Module for Go")
    (description
     "This module implements a cross-platform persistent token cache for
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity,azidentity}
credentials.  See that module's
@@url{https://pkg.go.dev/github.com/Azure/azure-sdk-for-go/sdk/azidentity#pkg-examples,examples}
for sample code showing how to configure persistent caching for a credential,
and its @@url{https://aka.ms/azsdk/go/identity/caching,token caching document}
for more information about the implementation.")
    (license license:expat)))

(define-public go-github-com-azuread-microsoft-authentication-library-for-go
  (package
    (name "go-github-com-azuread-microsoft-authentication-library-for-go")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/AzureAD/microsoft-authentication-library-for-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17sc8d4xajp9ni1m5vbiwajvh0siay3lmssm24hydzmalljrc3pd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/AzureAD/microsoft-authentication-library-for-go"))
    (propagated-inputs (list go-github-com-pkg-browser
                             go-github-com-montanaflynn-stats
                             go-github-com-kylelemons-godebug
                             go-github-com-google-uuid
                             go-github-com-golang-jwt-jwt-v5))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-library-for-go")
    (synopsis "Microsoft Authentication Library (MSAL) for Go")
    (description
     "The Microsoft Authentication Library (MSAL) for Go is part of the
@@url{https://aka.ms/aaddevv2,Microsoft identity platform for developers}
(formerly named Azure AD) v2.0.  It allows you to sign in users or apps with
Microsoft identities
(@@url{https://azure.microsoft.com/services/active-directory/,Azure AD} and
@@url{https://account.microsoft.com,Microsoft Accounts}) and obtain tokens to
call Microsoft APIs such as @@url{https://graph.microsoft.io/,Microsoft Graph}
or your own APIs registered with the Microsoft identity platform.  It is built
using industry standard OAuth2 and @code{OpenID} Connect protocols.")
    (license license:expat)))

(define-public go-github-com-bsm-ginkgo
  (package
    (name "go-github-com-bsm-ginkgo")
    (version "2.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/ginkgo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01k1j1bwdq23hs9zzbz9kdljvr6hzym53mqxh2gy0bz4lggcd6qs"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/ginkgo/v2"
      #:unpack-path "github.com/bsm/ginkgo"))
    (home-page "https://github.com/bsm/ginkgo")
    (synopsis "Ginkgo")
    (description
     "Ginkgo is a testing framework for Go designed to help you write expressive
tests. @@url{https://github.com/onsi/ginkgo,https://github.com/onsi/ginkgo}
MIT-Licensed.")
    (license license:expat)))

(define-public go-github-com-bsm-gomega
  (package
    (name "go-github-com-bsm-gomega")
    (version "1.27.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bsm/gomega")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7p85wsqv1j9aq052vdw006xq42n1rdgnk1lr6f5wnapwab2shz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bsm/gomega"))
    (home-page "https://github.com/bsm/gomega")
    (synopsis "Gomega")
    (description
     "Gomega is the Ginkgo BDD-style testing framework's preferred matcher library.")
    (license license:expat)))

(define-public go-github-com-redis-go-redis
  (package
    (name "go-github-com-redis-go-redis")
    (version "9.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/redis/go-redis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y4zd09yhbhr07idcny95ilh57vcva5kcsnnx2ffa3w4k2c33181"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/redis/go-redis/v9"
      #:unpack-path "github.com/redis/go-redis"))
    (propagated-inputs (list go-github-com-dgryski-go-rendezvous
                             go-github-com-cespare-xxhash-v2
                             go-github-com-bsm-gomega
                             go-github-com-bsm-ginkgo-v2))
    (home-page "https://github.com/redis/go-redis")
    (synopsis "Redis client for Go")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-azidentity
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-azidentity")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/azidentity"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f8zkxnrpadp62p9x6kgd9nfbnmppvgmvf2li78y590xykppdcds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/azidentity"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-crypto
                        go-github-com-stretchr-testify
                        go-github-com-redis-go-redis-v9
                        go-github-com-google-uuid
                        go-github-com-golang-jwt-jwt-v5
                        go-github-com-azuread-microsoft-authentication-library-for-go
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity-cache
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Identity Client Module for Go")
    (description
     "The Azure Identity module provides Microsoft Entra ID
(@@url{https://learn.microsoft.com/entra/fundamentals/new-name,formerly Azure
Active Directory}) token authentication support across the Azure SDK. It
includes a set of @@code{@code{TokenCredential}} implementations, which can be
used with Azure SDK clients supporting token authentication.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-internal
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-internal")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0czvydkb9sgk2sy1dwl78rbd81gjy5ykhpasrm24h4hhv382nyhz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/internal"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-golang-org-x-text go-golang-org-x-net
                        go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure.Core Internal Module for Go")
    (description "internal contains content for Azure SDK developers.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir
                                          "sdk/resourcemanager/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0db2hxq8wigvmqkj8h5gfq9lw6bp0k13hvcyi0hxgnriz1mdvpx5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/internal/v2"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir
                      "sdk/resourcemanager/managementgroups/armmanagementgroups"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l6jdkkgc2fhf43qd3d1s1llnv40skmql6zf9vjbyrc73qad44cj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/managementgroups/armmanagementgroups"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v2
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Management Groups Module for Go")
    (description
     "The @@code{armmanagementgroups} module provides operations for working with
Azure Management Groups.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir "sdk/resourcemanager/resources/armresources"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01r351rl6w0gdpgpllq692yip5c761l8f8098hfnw4bgsk625qc7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/resources/armresources"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-managementgroups-armmanagementgroups
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v2
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Resources Module for Go")
    (description
     "The @@code{armresources} module provides operations for working with Azure
Resources.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                      #:subdir "sdk/resourcemanager/storage/armstorage"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j2r6qjah11dl66kpgfzlni1g11zahg0ia0darrnll7f8x7jkpx0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/resourcemanager/storage/armstorage"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-resources-armresources
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-internal-v3
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Storage Module for Go")
    (description
     "The @@code{armstorage} module provides operations for working with Azure
Storage.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/storage/azblob"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w1cb6mgznxq81l81k4x3sayz0vp7f18ni9i7afsz23i40dfn130"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/Azure/azure-sdk-for-go/sdk/storage/azblob"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-stretchr-testify
                        go-github-com-azure-azure-sdk-for-go-sdk-resourcemanager-storage-armstorage
                        go-github-com-azure-azure-sdk-for-go-sdk-internal
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Blob Storage module for Go")
    (description
     "Azure Blob Storage is Microsoft's object storage solution for the cloud.  Blob
Storage is optimized for storing massive amounts of unstructured data - data
that does not adhere to a particular data model or definition, such as text or
binary data.  For more information, see
@@url{https://learn.microsoft.com/azure/storage/blobs/storage-blobs-introduction,Introduction
to Azure Blob Storage}.")
    (license license:expat)))

(define-public go-github-com-tonistiigi-go-actions-cache
  (package
    (name "go-github-com-tonistiigi-go-actions-cache")
    (version "0.0.0-20250611155157-388a2ec8cdf8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/go-actions-cache")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfdzqrmgk843068dq6w74ixy4mh0xq1b4p2a371mx3rsh0zjhbc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/tonistiigi/go-actions-cache"))
    (propagated-inputs (list go-golang-org-x-sync
                        go-github-com-stretchr-testify
                        go-github-com-pkg-errors
                        go-github-com-golang-jwt-jwt-v5
                        go-github-com-dimchansky-utfbom
                        go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob))
    (home-page "https://github.com/tonistiigi/go-actions-cache")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-tonistiigi-go-archvariant
  (package
    (name "go-github-com-tonistiigi-go-archvariant")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/go-archvariant")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kmchhx374ahd9a7p0qx3gq9cffxfkkgq503q95ygya986xkgnjz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/go-archvariant"))
    (home-page "https://github.com/tonistiigi/go-archvariant")
    (synopsis "go-archvariant")
    (description
     "Go package for determining the maximum compatibility version of the current
system.  The main use case is to use this value in container
@@url{https://github.com/containerd/containerd/raw/v1.5.9/platforms/platforms.go#L55,platform
definitions}.")
    (license license:expat)))

(define-public go-github-com-tonistiigi-go-csvvalue
  (package
    (name "go-github-com-tonistiigi-go-csvvalue")
    (version "0.0.0-20240814133006-030d3b2625d0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/go-csvvalue")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "128i7fmxyf08q80b1i6hgb3sbx2nsa56p2kr6vcdyijazhqnrn0p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/go-csvvalue"))
    (home-page "https://github.com/tonistiigi/go-csvvalue")
    (synopsis "go-csvvalue")
    (description
     "Package csvvalue provides an efficient parser for a single line CSV value.  It
is more efficient than the standard library csv package for parsing many small
values.  For multi-line CSV parsing, the standard library is recommended.")
    (license license:expat)))

(define-public go-github-com-tonistiigi-units
  (package
    (name "go-github-com-tonistiigi-units")
    (version "0.0.0-20180711220420-6950e57a87ea")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/units")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w8rgmlg6pim4vchg4qfpdf6niqmsp0a4f6bafgwd1gnwxi71zkf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/units"))
    (home-page "https://github.com/tonistiigi/units")
    (synopsis "Simple byte size formatting.")
    (description "Simple byte size formatting.")
    (license license:expat)))

(define-public go-github-com-tonistiigi-vt100
  (package
    (name "go-github-com-tonistiigi-vt100")
    (version "0.0.0-20240514184818-90bafcd6abab")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonistiigi/vt100")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vjk3yam610kc600h3hd3glsygr3m863765m9q7c0gsaj0vd38y0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/tonistiigi/vt100"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/tonistiigi/vt100")
    (synopsis #f)
    (description
     "package vt100 implements a quick-and-dirty programmable ANSI terminal emulator.")
    (license license:expat)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
  (package
    (name
     "go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc")
    (version "0.61.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                      #:subdir
                      "instrumentation/google.golang.org/grpc/otelgrpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/google.golang.org/grpc/otelgrpc"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-sdk
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description "Package otelgrpc is the instrumentation library for
@@url{/google.golang.org/grpc,google.golang.org/grpc}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-contrib-instrumentation-net-http-httptrace-otelhttptrace
  (package
    (name
     "go-go-opentelemetry-io-contrib-instrumentation-net-http-httptrace-otelhttptrace")
    (version "0.61.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go-contrib")
             (commit (go-version->git-ref version
                      #:subdir
                      "instrumentation/net/http/httptrace/otelhttptrace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15k2xps4qv37pw3ml6pj98kihl6a04bbr005px5hhckl23s40w2c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/contrib/instrumentation/net/http/httptrace/otelhttptrace"
      #:unpack-path "go.opentelemetry.io/contrib"))
    (propagated-inputs (list go-go-opentelemetry-io-otel-trace
                             go-go-opentelemetry-io-otel-metric
                             go-go-opentelemetry-io-otel
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp))
    (home-page "https://go.opentelemetry.io/contrib")
    (synopsis #f)
    (description "Package otelhttptrace provides instrumentation for the
@@url{/net/http/httptrace,net/http/httptrace} package.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-jaeger
  (package
    (name "go-go-opentelemetry-io-otel-exporters-jaeger")
    (version "1.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporters/jaeger"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0241rgx3wf4gfk9q0s0r378kv6hs8dii1d2zgp09941dgk59zmp9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.opentelemetry.io/otel/exporters/jaeger"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-go-logr-stdr
                             go-github-com-go-logr-logr))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OpenTelemetry-Go Jaeger Exporter")
    (description
     "Package jaeger contains an @code{OpenTelemetry} tracing exporter for Jaeger.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetricgrpc
  (package
    (name
     "go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetricgrpc")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlpmetric/otlpmetricgrpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetricgrpc"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-cenkalti-backoff-v5))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP Metric gRPC Exporter")
    (description
     "Package otlpmetricgrpc provides an OTLP metrics exporter using @code{gRPC}.  By
default the telemetry is sent to
@@url{https://localhost:4317,https://localhost:4317}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp
  (package
    (name
     "go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlpmetric/otlpmetrichttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlpmetric/otlpmetrichttp"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp
                             go-github-com-cenkalti-backoff-v5))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP Metric HTTP Exporter")
    (description
     "Package otlpmetrichttp provides an OTLP metrics exporter using HTTP with
protobuf payloads.  By default the telemetry is sent to
@@url{https://localhost:4318/v1/metrics,https://localhost:4318/v1/metrics}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
  (package
    (name "go-go-opentelemetry-io-otel-exporters-otlp-otlptrace")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporters/otlp/otlptrace"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/otel/exporters/otlp/otlptrace"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-stretchr-testify
                             go-github-com-google-go-cmp))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP Trace Exporter")
    (description
     "Package otlptrace contains abstractions for OTLP span exporters.  See the
official OTLP span exporter implementations:.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
  (package
    (name "go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlptrace/otlptracegrpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-go-uber-org-goleak
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-stretchr-testify
                             go-github-com-cenkalti-backoff-v5))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP Trace gRPC Exporter")
    (description
     "Package otlptracegrpc provides an OTLP span exporter using @code{gRPC}.  By
default the telemetry is sent to
@@url{https://localhost:4317,https://localhost:4317}.")
    (license license:asl2.0)))

(define-public go-github-com-cenkalti-backoff-v5
  (package
    (name "go-github-com-cenkalti-backoff")
    (version "5.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cenkalti/backoff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hc4manlkqfy9acva1jb8ayh8jihjb0h76l3g1sqqp0vlaq5y6q3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/cenkalti/backoff/v5"
      #:unpack-path "github.com/cenkalti/backoff"))
    (home-page "https://github.com/cenkalti/backoff")
    (synopsis "Exponential Backoff")
    (description
     "Package backoff implements backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
  (package
    (name "go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp")
    (version "1.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                      #:subdir "exporters/otlp/otlptrace/otlptracehttp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path
      "go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-go-opentelemetry-io-proto-otlp
                             go-github-com-stretchr-testify
                             go-github-com-cenkalti-backoff-v5))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "OTLP Trace HTTP Exporter")
    (description
     "Package otlptracehttp provides an OTLP span exporter using HTTP with protobuf
payloads.  By default the telemetry is sent to
@@url{https://localhost:4318/v1/traces,https://localhost:4318/v1/traces}.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-otel-exporters-prometheus
  (package
    (name "go-go-opentelemetry-io-otel-exporters-prometheus")
    (version "0.58.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-go")
             (commit (go-version->git-ref version
                                          #:subdir "exporters/prometheus"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kvfbqc56p1h9rh9cvgn37ya6k10613r0f2rhjiwrrkgs2mszk30"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/otel/exporters/prometheus"
      #:unpack-path "go.opentelemetry.io/otel"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-github-com-stretchr-testify
                             go-github-com-prometheus-common
                             go-github-com-prometheus-client-model
                             go-github-com-prometheus-client-golang))
    (home-page "https://go.opentelemetry.io/otel")
    (synopsis "Prometheus Exporter")
    (description
     "Package prometheus provides a Prometheus Exporter that converts OTLP metrics
into the Prometheus exposition format and implements prometheus.Collector to
provide a handler for these metrics.")
    (license license:asl2.0)))

(define-public go-google-golang-org-genproto-googleapis-api
  (package
    (name "go-google-golang-org-genproto-googleapis-api")
    (version "0.0.0-20250603155806-513f23925822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/go-genproto")
             (commit (go-version->git-ref version
                                          #:subdir "googleapis/api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgx8x5isyfhv6c6nl2ladgxzkx1k03m1if57rskvdc09w1q4yrx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "google.golang.org/genproto/googleapis/api"
      #:unpack-path "google.golang.org/genproto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc))
    (home-page "https://google.golang.org/genproto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-grpc-ecosystem-grpc-gateway-v2
  (package
    (name "go-github-com-grpc-ecosystem-grpc-gateway")
    (version "2.26.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc-ecosystem/grpc-gateway")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pw845x9bqhj64pxvyaafacq0mmmblbf5z4r2arprhdnb05czx3v"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/grpc-ecosystem/grpc-gateway/v2"
      #:unpack-path "github.com/grpc-ecosystem/grpc-gateway"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-genproto-googleapis-api
                             go-golang-org-x-text
                             go-golang-org-x-oauth2
                             go-github-com-rogpeppe-fastuuid
                             go-github-com-google-go-cmp
                             go-github-com-antihax-optional))
    (home-page "https://github.com/grpc-ecosystem/grpc-gateway")
    (synopsis "About")
    (description
     "The @code{gRPC-Gateway} is a plugin of the Google protocol buffers compiler
@@url{https://github.com/protocolbuffers/protobuf,protoc}.  It reads protobuf
service definitions and generates a reverse-proxy server which translates a
RESTful HTTP API into @code{gRPC}.  This server is generated according to the
@@url{https://github.com/googleapis/googleapis/raw/master/google/api/http.proto#L46,(code
google.api.http)} annotations in your service definitions.")
    (license license:bsd-3)))

(define-public go-go-opentelemetry-io-proto-otlp
  (package
    (name "go-go-opentelemetry-io-proto-otlp")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/open-telemetry/opentelemetry-proto-go")
             (commit (go-version->git-ref version
                                          #:subdir "otlp"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k7g5p1qhw17szyzxr08aixi5d0d2ixlb3sp4dksgz45v0dy5cds"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "go.opentelemetry.io/proto/otlp"
      #:unpack-path "go.opentelemetry.io/proto"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc
                             go-github-com-grpc-ecosystem-grpc-gateway-v2))
    (home-page "https://go.opentelemetry.io/proto")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-cmd-protoc-gen-go-grpc
  (package
    (name "go-google-golang-org-grpc-cmd-protoc-gen-go-grpc")
    (version "1.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grpc/grpc-go")
             (commit (go-version->git-ref version
                                          #:subdir "cmd/protoc-gen-go-grpc"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yn1ir5y0wc91q95ngr3dlz2cyhp0wlb9l30hkw2cr34r38hq19w"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "google.golang.org/grpc/cmd/protoc-gen-go-grpc"
      #:unpack-path "google.golang.org/grpc"))
    (propagated-inputs (list go-google-golang-org-protobuf
                             go-google-golang-org-grpc))
    (home-page "https://google.golang.org/grpc")
    (synopsis "protoc-gen-go-grpc")
    (description
     "protoc-gen-go-grpc is a plugin for the Google protocol buffer compiler to
generate Go code.  Install it by building this program and making it accessible
within your PATH with the name:.")
    (license license:asl2.0)))

(define-public go-kernel-org-pub-linux-libs-security-libcap-psx
  (package
    (name "go-kernel-org-pub-linux-libs-security-libcap-psx")
    (version "1.2.76")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/libcap/libcap.git")
             (commit (go-version->git-ref version
                                          #:subdir "psx"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0va0bkq5kxf0ccsdpw598vsmk4kdzhaafjvym0g5b2n49c5sn59b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "kernel.org/pub/linux/libs/security/libcap/psx"
      #:unpack-path "kernel.org/pub/linux/libs/security/libcap"))
    (home-page "https://kernel.org/pub/linux/libs/security/libcap")
    (synopsis "PSX")
    (description
     "Package psx provides support for system calls that are run simultaneously on all
threads under Linux.  It supports tool chains after go1.16.  Earlier toolchains
had no reliable way to support this because of
@@url{https://bugzilla.kernel.org/show_bug.cgi?id=219478,Bug 219478}.")
    (license license:gpl2)))

(define-public go-kernel-org-pub-linux-libs-security-libcap-cap
  (package
    (name "go-kernel-org-pub-linux-libs-security-libcap-cap")
    (version "1.2.76")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.kernel.org/pub/scm/libs/libcap/libcap.git")
             (commit (go-version->git-ref version
                                          #:subdir "cap"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0va0bkq5kxf0ccsdpw598vsmk4kdzhaafjvym0g5b2n49c5sn59b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "kernel.org/pub/linux/libs/security/libcap/cap"
      #:unpack-path "kernel.org/pub/linux/libs/security/libcap"))
    (propagated-inputs (list go-kernel-org-pub-linux-libs-security-libcap-psx))
    (home-page "https://kernel.org/pub/linux/libs/security/libcap")
    (synopsis #f)
    (description
     "Package cap provides all the Linux Capabilities userspace library API bindings
in native Go.")
    (license license:gpl2)))

(define-public go-github-com-opencontainers-runtime-tools
  (package
    (name "go-github-com-opencontainers-runtime-tools")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/opencontainers/runtime-tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pli3jb1rq9lkzzz83f7jw788vijg7x6ly3vgasdlwri7kiph1sa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/opencontainers/runtime-tools"))
    (home-page "https://github.com/opencontainers/runtime-tools")
    (synopsis "oci-runtime-tool")
    (description
     "oci-runtime-tool is a collection of tools for working with the
@@url{https://github.com/opencontainers/runtime-spec,OCI runtime specification}.
 To build from source code, runtime-tools requires Go 1.10.x or above.")
    (license license:asl2.0)))

(define-public go-tags-cncf-io-container-device-interface
  (package
    (name "go-tags-cncf-io-container-device-interface")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cncf-tags/container-device-interface")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wpp8fsrfjmgfkwwanakbigf68khwycdxbmr82k309fawfpbz7fj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "tags.cncf.io/container-device-interface"))
    (propagated-inputs (list go-sigs-k8s-io-yaml
                             go-gopkg-in-yaml-v3
                             go-golang-org-x-sys
                             go-github-com-stretchr-testify
                             go-github-com-opencontainers-runtime-tools
                             go-github-com-opencontainers-runtime-spec
                             go-github-com-fsnotify-fsnotify))
    (home-page "https://tags.cncf.io/container-device-interface")
    (synopsis "CDI - The Container Device Interface")
    (description
     "@@strong{NOTE:} The API for injecting CDI devices that existed at
@@code{container-device-interface/pkg} has been removed.  Users of this API
should migrate to the one at @@code{container-device-interface/pkg/cdi} as this
is actively maintained.")
    (license license:asl2.0)))

(define-public go-github-com-moby-buildkit
  (package
    (name "go-github-com-moby-buildkit")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/buildkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11ip0hp7g1a4yk68idac6vvz6np75k6mydvq7xlmcb6ff6p5dbvh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/moby/buildkit"))
    (propagated-inputs (list go-tags-cncf-io-container-device-interface
                        go-kernel-org-pub-linux-libs-security-libcap-cap
                        go-google-golang-org-protobuf
                        go-google-golang-org-grpc-cmd-protoc-gen-go-grpc
                        go-google-golang-org-grpc
                        go-google-golang-org-genproto-googleapis-rpc
                        go-golang-org-x-time
                        go-golang-org-x-sys
                        go-golang-org-x-sync
                        go-golang-org-x-net
                        go-golang-org-x-mod
                        go-golang-org-x-exp
                        go-golang-org-x-crypto
                        go-go-opentelemetry-io-proto-otlp
                        go-go-opentelemetry-io-otel-trace
                        go-go-opentelemetry-io-otel-sdk-metric
                        go-go-opentelemetry-io-otel-sdk
                        go-go-opentelemetry-io-otel-exporters-prometheus
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
                        go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
                        go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetrichttp
                        go-go-opentelemetry-io-otel-exporters-otlp-otlpmetric-otlpmetricgrpc
                        go-go-opentelemetry-io-otel-exporters-jaeger
                        go-go-opentelemetry-io-otel
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                        go-go-opentelemetry-io-contrib-instrumentation-net-http-httptrace-otelhttptrace
                        go-go-opentelemetry-io-contrib-instrumentation-google-golang-org-grpc-otelgrpc
                        go-go-etcd-io-bbolt
                        go-github-com-vishvananda-netlink
                        go-github-com-urfave-cli
                        go-github-com-tonistiigi-vt100
                        go-github-com-tonistiigi-units
                        go-github-com-tonistiigi-go-csvvalue
                        go-github-com-tonistiigi-go-archvariant
                        go-github-com-tonistiigi-go-actions-cache
                        go-github-com-tonistiigi-fsutil
                        go-github-com-tonistiigi-dchapes-mode
                        go-github-com-stretchr-testify
                        go-github-com-spdx-tools-golang
                        go-github-com-sirupsen-logrus
                        go-github-com-serialx-hashring
                        go-github-com-prometheus-procfs
                        go-github-com-prometheus-client-golang
                        go-github-com-planetscale-vtprotobuf
                        go-github-com-pkg-profile
                        go-github-com-pkg-errors
                        go-github-com-pelletier-go-toml
                        go-github-com-package-url-packageurl-go
                        go-github-com-opencontainers-selinux
                        go-github-com-opencontainers-runtime-spec
                        go-github-com-opencontainers-image-spec
                        go-github-com-opencontainers-go-digest
                        go-github-com-morikuni-aec
                        go-github-com-moby-sys-userns
                        go-github-com-moby-sys-user
                        go-github-com-moby-sys-signal
                        go-github-com-moby-sys-reexec
                        go-github-com-moby-sys-mountinfo
                        go-github-com-moby-patternmatcher
                        go-github-com-moby-locker
                        go-github-com-moby-go-archive
                        go-github-com-moby-docker-image-spec
                        go-github-com-mitchellh-hashstructure-v2
                        go-github-com-klauspost-compress
                        go-github-com-in-toto-in-toto-golang
                        go-github-com-hashicorp-golang-lru-v2
                        go-github-com-hashicorp-go-multierror
                        go-github-com-hashicorp-go-immutable-radix-v2
                        go-github-com-hashicorp-go-cleanhttp
                        go-github-com-google-shlex
                        go-github-com-google-go-cmp
                        go-github-com-golang-protobuf
                        go-github-com-gofrs-flock
                        go-github-com-docker-go-units
                        go-github-com-docker-go-connections
                        go-github-com-docker-docker
                        go-github-com-docker-cli
                        go-github-com-distribution-reference
                        go-github-com-coreos-go-systemd-v22
                        go-github-com-containernetworking-plugins
                        go-github-com-containerd-typeurl-v2
                        go-github-com-containerd-stargz-snapshotter-estargz
                        go-github-com-containerd-stargz-snapshotter
                        go-github-com-containerd-platforms
                        go-github-com-containerd-nydus-snapshotter
                        go-github-com-containerd-log
                        go-github-com-containerd-go-runc
                        go-github-com-containerd-go-cni
                        go-github-com-containerd-fuse-overlayfs-snapshotter-v2
                        go-github-com-containerd-errdefs
                        go-github-com-containerd-continuity
                        go-github-com-containerd-containerd-v2
                        go-github-com-containerd-containerd-api
                        go-github-com-containerd-console
                        go-github-com-containerd-accelerated-container-image
                        go-github-com-aws-aws-sdk-go-v2-service-s3
                        go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
                        go-github-com-aws-aws-sdk-go-v2-credentials
                        go-github-com-aws-aws-sdk-go-v2-config
                        go-github-com-aws-aws-sdk-go-v2
                        go-github-com-armon-circbuf
                        go-github-com-agext-levenshtein
                        go-github-com-microsoft-hcsshim
                        go-github-com-microsoft-go-winio
                        go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
                        go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                        go-github-com-azure-azure-sdk-for-go-sdk-azcore))
    (home-page "https://github.com/moby/buildkit")
    (synopsis "BuildKit")
    (description
     "@code{BuildKit} is a toolkit for converting source code to build artifacts in an
efficient, expressive and repeatable manner.")
    (license license:asl2.0)))

(define-public go-github-com-moby-patternmatcher
  (package
    (name "go-github-com-moby-patternmatcher")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/patternmatcher")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s77wpsc6szr9qdpnpg9q65ibgjgj4b2d12hwf6wrwb39grcnbcz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/moby/patternmatcher"))
    (home-page "https://github.com/moby/patternmatcher")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-rhysd-actionlint
  (package
    (name "go-github-com-rhysd-actionlint")
    (version "1.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rhysd/actionlint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xbsrcvklxn0lppikabwrizav945jk85d0mz16zc3spxc80plrvn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rhysd/actionlint"))
    (propagated-inputs (list go-gopkg-in-yaml-v3
                             go-golang-org-x-sys
                             go-golang-org-x-sync
                             go-github-com-yuin-goldmark
                             go-github-com-robfig-cron-v3
                             go-github-com-mattn-go-shellwords
                             go-github-com-mattn-go-runewidth
                             go-github-com-mattn-go-colorable
                             go-github-com-google-go-cmp
                             go-github-com-fatih-color
                             go-github-com-bmatcuk-doublestar-v4))
    (home-page "https://github.com/rhysd/actionlint")
    (synopsis "actionlint")
    (description
     "Package actionlint is the implementation of actionlint linter.  It's a static
checker for @code{GitHub} Actions workflow files.")
    (license license:expat)))

(define-public go-github-com-timshannon-bolthold
  (package
    (name "go-github-com-timshannon-bolthold")
    (version "0.0.0-20240314194003-30aac6950928")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/timshannon/bolthold")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107r4nwhvpdp0n9b5fls1lw8z8qsiajiykkpjs7947nrbc07ij1j"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/timshannon/bolthold"))
    (propagated-inputs (list go-go-etcd-io-bbolt))
    (home-page "https://github.com/timshannon/bolthold")
    (synopsis "BoltHold")
    (description
     "Package bolthold is an indexing and querying layer on top of a Bolt DB. The goal
is to allow easy, persistent storage and retrieval of Go types. @code{BoltDB} is
an embedded key-value store, and bolthold servers a similar use case however
with a higher level interface for common uses of @code{BoltDB}.")
    (license license:expat)))

(define-public go-github-com-masterminds-semver
  (package
    (name "go-github-com-masterminds-semver")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Masterminds/semver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i169xscsxsh8lsw8bz2apnsqixld37xdnfh36i30xy5wnf0iwfx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Masterminds/semver"))
    (home-page "https://github.com/Masterminds/semver")
    (synopsis "SemVer")
    (description
     "Package semver provides the ability to work with Semantic Versions
(@@url{http://semver.org,http://semver.org}) in Go.")
    (license license:expat)))

(define-public go-code-forgejo-org-forgejo-act
  (package
    (name "go-code-forgejo-org-forgejo-act")
    (version "1.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://code.forgejo.org/forgejo/act.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09k9y45fyviyg43il04v932fjyydic3c94qj42750ig3mz5xhn59"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "code.forgejo.org/forgejo/act"))
    (propagated-inputs (list go-github-com-gobwas-glob
                             go-github-com-masterminds-semver
                             go-gotest-tools-v3
                             go-gopkg-in-yaml-v3
                             go-golang-org-x-term
                             go-go-etcd-io-bbolt
                             go-github-com-timshannon-bolthold
                             go-github-com-stretchr-testify
                             go-github-com-spf13-pflag
                             go-github-com-spf13-cobra
                             go-github-com-sirupsen-logrus
                             go-github-com-sabhiram-go-gitignore
                             go-github-com-rhysd-actionlint
                             go-github-com-pkg-errors
                             go-github-com-opencontainers-selinux
                             go-github-com-opencontainers-image-spec
                             go-github-com-moby-patternmatcher
                             go-github-com-moby-buildkit
                             go-github-com-mattn-go-isatty
                             go-github-com-kballard-go-shellquote
                             go-github-com-julienschmidt-httprouter
                             go-github-com-joho-godotenv
                             go-github-com-imdario-mergo
                             go-github-com-go-git-go-git-v5
                             go-github-com-go-git-go-billy-v5
                             go-github-com-docker-go-connections
                             go-github-com-docker-docker
                             go-github-com-docker-cli
                             go-github-com-distribution-reference
                             go-github-com-creack-pty
                             go-github-com-andreaskoch-go-fswatch
                             go-github-com-adrg-xdg
                             go-github-com-alecaivazis-survey-v2))
    (home-page "https://code.forgejo.org/forgejo/act")
    (synopsis "Overview")
    (description
     "Run your @@url{https://developer.github.com/actions/,@code{GitHub} Actions}
locally! Why would you want to do this? Two reasons:.")
    (license license:expat)))

(define-public go-mvdan-cc-xurls-v2
  (package
    (name "go-mvdan-cc-xurls")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mvdan/xurls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kk4mjizr23zjzsavs21krp13w52p3b8dcm4ahlrr6xkkfn8ry3i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "mvdan.cc/xurls/v2"
      #:unpack-path "mvdan.cc/xurls"))
    (propagated-inputs (list go-golang-org-x-sync go-golang-org-x-mod
                             go-github-com-rogpeppe-go-internal))
    (home-page "https://mvdan.cc/xurls")
    (synopsis "xurls")
    (description
     "Package xurls extracts urls from plain text using regular expressions.")
    (license license:bsd-3)))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.36.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lp1a6bcwdiil4my0aq85ranxf2k757m8q0ss9658jyrh5g7av79"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "google.golang.org/protobuf"))
    (propagated-inputs (list go-github-com-google-go-cmp
                             go-github-com-golang-protobuf))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
     "This project hosts the Go implementation for @@url{https://protobuf.dev,protocol
buffers}, which is a language-neutral, platform-neutral, extensible mechanism
for serializing structured data.  The protocol buffer language is a language for
specifying the schema for structured data.  This schema is compiled into
language specific bindings.  This project provides both a tool to generate Go
code for the protocol buffer language, and also the runtime implementation to
handle serialization of messages in Go.  See the
@@url{https://protobuf.dev/overview,protocol buffer developer guide} for more
information about protocol buffers themselves.")
    (license license:bsd-3)))

(define-public go-gopkg-in-gomail-v2
  (package
    (name "go-github-com-go-gomail-gomail")
    (version "0.0.0-20160411212932-81ebce5c23df")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-gomail/gomail")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zdykrv5s19lnq0g49p6njldy4cpk4g161vyjafiw7f84h8r28mc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-gomail/gomail"))
    (home-page "https://github.com/go-gomail/gomail")
    (synopsis "Gomail")
    (description
     "Package gomail provides a simple interface to compose emails and to mail them
efficiently.")
    (license license:expat)))

(define-public go-gopkg-in-ini-v1
  (package
    (name "go-github-com-go-ini-ini")
    (version "1.67.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-ini/ini")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vpzkjmrwp7bqqsijp61293kk2vn6lcck56j8m5y6ks6cf21lpap"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-ini/ini"))
    (home-page "https://github.com/go-ini/ini")
    (synopsis "INI")
    (description
     "Package ini provides INI file read and write functionality in Go.")
    (license license:asl2.0)))

(define-public go-gopkg-in-yaml-v3
  (package
    (name "go-github-com-go-yaml-yaml")
    (version "0.0.0-20250401170010-944c86a7d293")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-yaml/yaml")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bdfmwqz2z38rmcalvrqgng01ria2g47v1sjnw60b4cmqxzpdhxb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-yaml/yaml"))
    (propagated-inputs (list go-gopkg-in-check-v1))
    (home-page "https://github.com/go-yaml/yaml")
    (synopsis "YAML support for the Go language")
    (description "Package yaml implements YAML support for the Go language.")
    (license license:asl2.0)))

(define-public go-github-com-yuin-goldmark-highlighting-v2
  (package
    (name "go-github-com-yuin-goldmark-highlighting")
    (version "0.0.0-20220208100518-594be1970594")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yuin/goldmark-highlighting")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vy75xz2lmsmi694ikd94k9sgpwpnp0n82lvil80jn1854vxva6n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yuin/goldmark-highlighting"))
    (propagated-inputs (list go-github-com-yuin-goldmark
                             go-github-com-alecthomas-chroma))
    (home-page "https://github.com/yuin/goldmark-highlighting")
    (synopsis "goldmark-highlighting")
    (description
     "package highlighting is a extension for the
goldmark(@@url{http://github.com/yuin/goldmark,http://github.com/yuin/goldmark}).")
    (license license:expat)))

(define-public forgejo
  (package
    (name "forgejo")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/forgejo/forgejo.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03wbi2ra13zchms6p93nzqrn2klxqz2hdw366xv624awnx7js72b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "codeberg.org/forgejo/forgejo"))
    (propagated-inputs
     (list go-code-forgejo-org-forgejo-act
           go-github-com-lunny-vfsgen
           go-github-com-6543-go-version
           go-xorm-io-xorm
           go-xorm-io-builder
           go-strk-kbt-io-projects-go-libravatar
           go-mvdan-cc-xurls-v2
           go-gopkg-in-yaml-v3
           go-gopkg-in-ini-v1
           go-gopkg-in-gomail-v2
           go-google-golang-org-protobuf
           go-google-golang-org-grpc
           go-golang-org-x-tools
           go-golang-org-x-text
           go-golang-org-x-sys
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-golang-org-x-image
           go-golang-org-x-crypto
           go-github-com-yuin-goldmark-meta
           go-github-com-yuin-goldmark-highlighting-v2
           go-github-com-yuin-goldmark
           go-github-com-yohcop-openid-go
           go-github-com-xeipuuv-gojsonschema
           go-github-com-xanzy-go-gitlab
           go-github-com-urfave-cli
           go-github-com-ulikunitz-xz
           go-github-com-tstranex-u2f
           go-github-com-syndtr-goleveldb
           go-github-com-stretchr-testify
           go-github-com-sergi-go-diff
           go-github-com-sassoftware-go-rpmutils
           go-github-com-santhosh-tekuri-jsonschema-v5
           go-github-com-redis-go-redis-v9
           go-github-com-quasoft-websspi
           go-github-com-prometheus-client-golang
           go-github-com-pquerna-otp
           go-github-com-pkg-errors
           go-github-com-opencontainers-image-spec
           go-github-com-opencontainers-go-digest
           go-github-com-olivere-elastic-v7
           go-github-com-oliamb-cutter
           go-github-com-niklasfasching-go-org
           go-github-com-nfnt-resize
           go-github-com-msteinert-pam
           go-github-com-minio-sha256-simd
           go-github-com-minio-minio-go-v7
           go-github-com-microcosm-cc-bluemonday
           go-github-com-mholt-archiver-v3
           go-github-com-meilisearch-meilisearch-go
           go-github-com-mattn-go-sqlite3
           go-github-com-mattn-go-isatty
           go-github-com-markbates-goth
           go-github-com-lib-pq
           go-github-com-klauspost-cpuid-v2
           go-github-com-klauspost-compress
           go-github-com-keybase-go-crypto
           go-github-com-kballard-go-shellquote
           go-github-com-json-iterator-go
           go-github-com-jhillyerd-enmime
           go-github-com-jaytaylor-html2text
           go-github-com-huandu-xstrings
           go-github-com-hashicorp-golang-lru
           go-github-com-gorilla-sessions
           go-github-com-gorilla-feeds
           go-github-com-google-uuid
           go-github-com-google-pprof
           go-github-com-google-go-github-v52
           go-github-com-golang-jwt-jwt-v4
           go-github-com-gogs-go-gogs-client
           go-github-com-gogs-cron
           go-github-com-gogs-chardet
           go-github-com-gobwas-glob
           go-github-com-go-webauthn-webauthn
           go-github-com-go-testfixtures-testfixtures-v3
           go-github-com-go-swagger-go-swagger
           go-github-com-go-sql-driver-mysql
           go-github-com-go-ldap-ldap-v3
           go-github-com-go-git-go-git-v5
           go-github-com-go-git-go-billy-v5
           go-github-com-go-fed-httpsig
           go-github-com-go-enry-go-enry-v2
           go-github-com-go-chi-cors
           go-github-com-go-chi-chi-v5
           go-github-com-go-ap-jsonld
           go-github-com-go-ap-activitypub
           go-github-com-gliderlabs-ssh
           go-github-com-fsnotify-fsnotify
           go-github-com-felixge-fgprof
           go-github-com-ethantkoenig-rupture
           go-github-com-emirpasic-gods
           go-github-com-emersion-go-imap
           go-github-com-editorconfig-editorconfig-core-go-v2
           go-github-com-dustin-go-humanize
           go-github-com-dsnet-compress
           go-github-com-djherbis-nio-v3
           go-github-com-djherbis-buffer
           go-github-com-dimiro1-reply
           go-github-com-denisenkom-go-mssqldb
           go-github-com-chi-middleware-proxy
           go-github-com-caddyserver-certmagic
           go-github-com-buildkite-terminal-to-html-v3
           go-github-com-bufbuild-connect-go
           go-github-com-blevesearch-bleve-v2
           go-github-com-blakesmith-ar
           go-github-com-alecthomas-chroma-v2
           go-github-com-puerkitobio-goquery
           go-github-com-nytimes-gziphandler
           go-github-com-azure-go-ntlmssp
           go-github-com-42wim-sshsig
           go-gitea-com-lunny-levelqueue
           go-gitea-com-lunny-dingtalk-webhook
           go-gitea-com-go-chi-session
           go-gitea-com-go-chi-captcha
           go-gitea-com-go-chi-cache
           go-gitea-com-go-chi-binding
           go-codeberg-org-gusted-mcaptcha
           go-code-gitea-io-sdk-gitea
           go-code-gitea-io-gitea-vet
           go-code-gitea-io-actions-proto-go))
    (home-page "https://codeberg.org/forgejo/forgejo")
    (synopsis "Gitea - Git with a cup of tea")
    (description
     "Gitea (git with a cup of tea) is a painless self-hosted Git Service.")
    (license license:expat)))
