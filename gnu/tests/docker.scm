;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu tests docker)
  #:use-module (gnu image)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages)
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages docker)
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module ((guix scripts pack) #:prefix pack:)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (%test-docker
            %test-docker-system
            %test-oci-container))

(define %docker-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service containerd-service-type)
   (service docker-service-type)))

(define (run-docker-test docker-tarball)
  "Load DOCKER-TARBALL as Docker image and run it in a Docker container,
inside %DOCKER-OS."
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %docker-os
      (list docker-tarball))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "docker")

          (test-assert "containerd service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'containerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-assert "containerd PID file present"
            (wait-for-file "/run/containerd/containerd.pid" marionette))

          (test-assert "dockerd service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-eq "fetch version"
            0
            (marionette-eval
             `(begin
                (system* ,(string-append #$docker-cli "/bin/docker")
                         "version"))
             marionette))

          (test-equal "Load docker image and run it"
            '("hello world" "hi!" "JSON!" #o1777)
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen)
                             (ice-9 rdelim))

                (define slurp
                  (lambda args
                    (let* ((port (apply open-pipe* OPEN_READ args))
                           (output (read-line port))
                           (status (close-pipe port)))
                      output)))
                (let* ((raw-line (slurp ,(string-append #$docker-cli
                                                        "/bin/docker")
                                                        "load" "-i"
                                                        ,#$docker-tarball))
                       (repository&tag (string-drop raw-line
                                                    (string-length
                                                     "Loaded image: ")))
                       (response1 (slurp
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "run" "--entrypoint" "bin/Guile"
                                   repository&tag
                                   "/aa.scm"))
                       (response2 (slurp          ;default entry point
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "run" repository&tag
                                   "-c" "(display \"hi!\")"))

                       ;; Check whether (json) is in $GUILE_LOAD_PATH.
                       (response3 (slurp    ;default entry point + environment
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "run" repository&tag
                                   "-c" "(use-modules (json))
  (display (json-string->scm (scm->json-string \"JSON!\")))"))

                       ;; Check whether /tmp exists.
                       (response4 (slurp
                                   ,(string-append #$docker-cli "/bin/docker")
                                   "run" repository&tag "-c"
                                   "(display (stat:perms (lstat \"/tmp\")))")))
                  (list response1 response2 response3
                        (string->number response4))))
             marionette))

          (test-end))))

  (gexp->derivation "docker-test" test))

(define (build-tarball&run-docker-test)
  (mlet* %store-monad
      ((_ (set-grafting #f))
       (guile (set-guile-for-build (default-guile)))
       (guest-script-package ->
        (package
          (name "guest-script")
          (version "0")
          (source #f)
          (build-system trivial-build-system)
          (arguments `(#:guile ,guile-3.0
                       #:builder
                       (let ((out (assoc-ref %outputs "out")))
                         (mkdir out)
                         (call-with-output-file (string-append out "/a.scm")
                           (lambda (port)
                             (display "(display \"hello world\n\")" port)))
                         #t)))
          (synopsis "Display hello world using Guile")
          (description "This package displays the text \"hello world\" on the
standard output device and then enters a new line.")
          (home-page #f)
          (license license:public-domain)))
       (profile (profile-derivation (packages->manifest
                                     (list guile-3.0 guile-json-3
                                           guest-script-package))
                                    #:hooks '()
                                    #:locales? #f))
       (tarball (pack:docker-image
                 "docker-pack" profile
                 #:symlinks '(("/bin/Guile" -> "bin/guile")
                              ("aa.scm" -> "a.scm"))
                 #:entry-point "bin/guile"
                 #:localstatedir? #t)))
    (run-docker-test tarball)))

(define %test-docker
  (system-test
   (name "docker")
   (description "Test Docker container of Guix.")
   (value (build-tarball&run-docker-test))))


(define (run-docker-system-test tarball)
  "Load DOCKER-TARBALL as Docker image and run it in a Docker container,
inside %DOCKER-OS."
  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %docker-os
      (list tarball))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (disk-image-size (* 6000 (expt 2 20)))
     (memory-size 2048)
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (guix build utils))

          (define marionette
            ;; Relax timeout to accommodate older systems.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "docker")

          (test-assert "containerd service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'containerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-assert "containerd PID file present"
            (wait-for-file "/run/containerd/containerd.pid" marionette))

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-assert "load system image and run it"
            (marionette-eval
             `(begin
                (use-modules (ice-9 popen)
                             (ice-9 rdelim)
                             (guix build utils))

                (define (slurp command . args)
                  ;; Return the output from COMMAND.
                  (let* ((port (apply open-pipe* OPEN_READ command args))
                         (output (read-line port))
                         (status (close-pipe port)))
                    output))

                (define (docker-cli command . args)
                  ;; Run the given Docker COMMAND.
                  (apply invoke #$(file-append docker-cli "/bin/docker")
                         command args))

                (define (wait-for-container-file container file)
                  ;; Wait for FILE to show up in CONTAINER.
                  (docker-cli "exec" container
                              #$(file-append guile-3.0 "/bin/guile")
                              "-c"
                              (object->string
                               `(let loop ((n 15))
                                  (when (zero? n)
                                    (error "file didn't show up" ,file))
                                  (unless (file-exists? ,file)
                                    (sleep 1)
                                    (loop (- n 1)))))))

                (let* ((line (slurp #$(file-append docker-cli "/bin/docker")
                                    "load" "-i" #$tarball))
                       (repository&tag (string-drop line
                                                    (string-length
                                                     "Loaded image: ")))
                       (container (slurp
                                   #$(file-append docker-cli "/bin/docker")
                                   "create" repository&tag)))
                  (docker-cli "start" container)

                  ;; Wait for shepherd to be ready.
                  (wait-for-container-file container
                                           "/var/run/shepherd/socket")

                  (docker-cli "exec" container
                              "/run/current-system/profile/bin/herd"
                              "status")
                  (slurp #$(file-append docker-cli "/bin/docker")
                         "exec" container
                         "/run/current-system/profile/bin/herd"
                         "status" "guix-daemon")))
             marionette))

          (test-end))))

  (gexp->derivation "docker-system-test" test))

(define %test-docker-system
  (system-test
   (name "docker-system")
   (description "Run a system image as produced by @command{guix system
docker-image} inside Docker.")
   (value (with-monad %store-monad
            (>>= (lower-object
                  (system-image (os->image
                                 (operating-system
                                   (inherit (simple-operating-system))
                                   ;; Use locales for a single libc to
                                   ;; reduce space requirements.
                                   (locale-libcs (list glibc)))
                                 #:type docker-image-type)))
                 run-docker-system-test)))))


(define %oci-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service containerd-service-type)
   (service docker-service-type)
   (extra-special-file "/shared.txt"
                       (plain-file "shared.txt" "hello"))
   (service oci-container-service-type
            (list
             (oci-container-configuration
              (image
               (oci-image
                (repository "guile")
                (value
                 (specifications->manifest '("guile")))
                (pack-options
                 '(#:symlinks (("/bin" -> "bin"))))))
              (entrypoint
               "/bin/guile")
              (command
               '("-c" "(let l ((c 300))(display c)(sleep 1)(when(positive? c)(l (- c 1))))"))
              (host-environment
               '(("VARIABLE" . "value")))
              (volumes
               '(("/shared.txt" . "/shared.txt:ro")))
              (extra-arguments
               '("--env" "VARIABLE")))))))

(define (run-oci-container-test)
  "Run IMAGE as an OCI backed Shepherd service, inside OS."

  (define os
    (marionette-operating-system
     (operating-system-with-gc-roots
      %oci-os
      (list))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (volatile? #f)
     (memory-size 1024)
     (disk-image-size (* 3000 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            ;; Relax timeout to accommodate older systems and
            ;; allow for pulling the image.
            (make-marionette (list #$vm) #:timeout 60))

          (test-runner-current (system-test-runner #$output))
          (test-begin "oci-container")

          (wait-for-file "/run/containerd/containerd.pid" marionette)

          (test-assert "docker-guile running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'docker-guile #:timeout 120)
                #t)
             marionette))

          (test-assert "passing host environment variables and volumes"
            (begin
              (define (run-test)
                (marionette-eval
                 `(begin
                    (use-modules (ice-9 popen)
                                 (ice-9 rdelim))

                    (define slurp
                      (lambda args
                        (let* ((port (apply open-pipe* OPEN_READ args))
                               (output (let ((line (read-line port)))
                                         (if (eof-object? line)
                                             ""
                                             line)))
                               (status (close-pipe port)))
                          output)))
                    (let* ((response1 (slurp
                                       ,(string-append #$docker-cli "/bin/docker")
                                       "exec" "docker-guile"
                                       "/bin/guile" "-c" "(display (getenv \"VARIABLE\"))"))
                           (response2 (slurp
                                       ,(string-append #$docker-cli "/bin/docker")
                                       "exec" "docker-guile"
                                       "/bin/guile" "-c" "(begin (use-modules (ice-9 popen) (ice-9 rdelim))
(display (call-with-input-file \"/shared.txt\" read-line)))")))
                      (list response1 response2)))
                 marionette))
              ;; Allow services to come up on slower machines
              (let loop ((attempts 0))
                (if (= attempts 60)
                    (error "Service didn't come up after more than 60 seconds")
                    (if (equal? '("value" "hello")
                                (run-test))
                        #t
                        (begin
                          (sleep 1)
                          (loop (+ 1 attempts))))))))

          (test-end))))

  (gexp->derivation "oci-container-test" test))

(define %test-oci-container
  (system-test
   (name "oci-container")
   (description "Test OCI backed Shepherd service.")
   (value (run-oci-container-test))))
