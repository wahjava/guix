;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2024, 2025 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu tests deploy)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:export (%test-deploy
            %test-rollback))

;;; Commentary:
;;;
;;; Test in-place system deployment: advancing the system generation on
;;; a running instance of the Guix System.
;;;
;;; Code:

(define (machines os-source)
  (scheme-file "machines.scm"
               #~(begin (use-modules (gnu machine ssh)
                                     (guix utils)
                                     (ice-9 textual-ports))
                        ;; XXX: (guix platforms ...) are not found in %load-path.
                        (set! (@ (guix platform) systems)
                              (compose list %current-system))
                        (list (machine
                                (configuration
                                 (machine-ssh-configuration
                                   (host-name "localhost")
                                   (host-key
                                    (call-with-input-file "/etc/ssh/ssh_host_ed25519_key.pub"
                                      get-string-all))
                                   (system (%current-system))))
                                (environment managed-host-environment-type)
                                (operating-system #$os-source))))))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (_ #f)))

(define (deploy-program os-source)
  (program-file "deploy.scm"
    (with-extensions (list guile-gcrypt guile-ssh)
      (with-imported-modules
        `(((guix config) => ,(make-config.scm)))
        #~(execl #$(file-append (current-guix) "/bin/guix")
                 "guix" "deploy" #$(machines os-source))))))

(define os
  (marionette-operating-system
    (simple-operating-system
       (service openssh-service-type
         (openssh-configuration
           (permit-root-login #t)
           (allow-empty-passwords? #t)))
       (service static-networking-service-type
         (list (static-networking
                 (inherit %loopback-static-networking)
                 (provision '(networking))))))
    #:imported-modules '((gnu services herd)
                         (guix combinators))))

(define vm (virtual-machine os))

(define system-generations-definition
  #~(define (system-generations marionette)
      "Return the names of the generation symlinks on MARIONETTE."
      (marionette-eval
        '(begin (use-modules (ice-9 ftw))
                (define (select? entry)
                  (not (member entry '("per-user" "system" "." ".."))))
                (scandir "/var/guix/profiles/" select?))
        marionette)))

(define* (run-deploy-test)
  "Run a test of an OS running DEPLOY-PROGRAM, which creates a new
generation of the system profile."
  (define new-os-source
    '(begin
       (use-modules (gnu tests))
       (operating-system
         (inherit %simple-os)
         (host-name (substring (operating-system-host-name %simple-os)
                               0 1)))))

  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 match)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          #$system-generations-definition

          (test-runner-current (system-test-runner #$output))
          (test-begin "deploy")

          (let ((generations-prior (system-generations marionette)))
            (test-assert "script successfully evaluated"
              (marionette-eval
               '(primitive-load #$script)
               marionette))

            (test-equal "script created new generation"
              (length (system-generations marionette))
              (1+ (length generations-prior)))

            (test-equal "script activated the new generation"
              (string-append "/var/guix/profiles/system-"
                             (number->string (+ 1 (length generations-prior)))
                             "-link")
              (marionette-eval '(readlink "/run/current-system")
                               marionette)))

          (test-assert "uname"
            (match (marionette-eval '(uname) marionette)
              (#("Linux" host-name _ ...)
               (string=? host-name #$(operating-system-host-name os)))))

          (test-end))))

  (gexp->derivation "deploy" (test (deploy-program new-os-source))))

(define* (run-rollback-test)
  "Run a test of an OS with a faulty bootloader running DEPLOY-PROGRAM,
which causes a rollback."
  (define bad-os-source
    '(begin
       (use-modules (gnu tests))
       (operating-system
         (inherit %simple-os)
         (host-name (substring (operating-system-host-name %simple-os)
                               0 1))
         (bootloader
          (bootloader-configuration
            (inherit (operating-system-bootloader %simple-os))
            (targets '("/dev/null")))))))

  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 match)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          #$system-generations-definition

          (test-runner-current (system-test-runner #$output))
          (test-begin "rollback")

          (let ((generations-prior (system-generations marionette)))
            (test-assert "script successfully evaluated"
              (marionette-eval
               '(primitive-load #$script)
               marionette))

            (test-equal "script created new generation"
              (length (system-generations marionette))
              (1+ (length generations-prior)))

            (test-equal "script rolled back the new generation"
              (string-append "/var/guix/profiles/system-"
                             (number->string (length generations-prior))
                             "-link")
              (marionette-eval '(readlink "/run/current-system")
                               marionette)))

          (test-assert "uname"
            (match (marionette-eval '(uname) marionette)
              (#("Linux" host-name _ ...)
               (string=? host-name #$(operating-system-host-name os)))))

          (test-end))))

  (gexp->derivation "rollback" (test (deploy-program bad-os-source))))

(define %test-deploy
  (system-test
   (name "deploy")
   (description "Deploy to the local machine.")
   (value (run-deploy-test))))

(define %test-rollback
  (system-test
   (name "deploy-rollback")
   (description "Rollback guix deploy with a faulty bootloader.")
   (value (run-rollback-test))))
