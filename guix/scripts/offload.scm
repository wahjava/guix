;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts offload)
  #:use-module (guix offload)
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module ((guix build syscalls) #:select (set-thread-name))
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (guix-offload))


;;; Commentary:
;;;
;;; Attempt to offload builds to the machines listed in
;;; /etc/guix/machines.scm, transferring missing dependencies over SSH, and
;;; retrieving the build output(s) over SSH upon success.
;;;
;;; This command should not be used directly; instead, it is called on-demand
;;; by the daemon, unless it was started with '--no-offload' or a client
;;; inhibited build hooks.
;;;
;;; Code:


;;;
;;; Entry point.
;;;

(define-command (guix-offload . args)
  (category plumbing)
  (synopsis "set up and operate build offloading")

  (define request-line-rx
    ;; The request format.  See 'tryBuildHook' method in build.cc.
    (make-regexp "([01]) ([a-z0-9_-]+) (/[[:graph:]]+.drv) ([[:graph:]]*)"))

  (define not-coma
    (char-set-complement (char-set #\,)))

  ;; Make sure $HOME really corresponds to the current user.  This is
  ;; necessary since lsh uses that to determine the location of the yarrow
  ;; seed file, and fails if it's owned by someone else.
  (and=> (passwd:dir (getpw (getuid)))
         (cut setenv "HOME" <>))

  (match args
    ((system max-silent-time print-build-trace? build-timeout)
     (let ((max-silent-time    (string->number max-silent-time))
           (build-timeout      (string->number build-timeout))
           (print-build-trace? (string=? print-build-trace? "1")))
       (set-thread-name "guix offload")
       (parameterize ((%current-system system))
         (let loop ((line (read-line)))
           (unless (eof-object? line)
             (cond ((regexp-exec request-line-rx line)
                    =>
                    (lambda (match)
                      (with-error-handling
                        (process-build-request
                         (equal? (match:substring match 1) "1")
                         (match:substring match 2) ; system
                         (match:substring match 3)
                         (string-tokenize
                          (match:substring match 4) not-coma)
                         #:print-build-trace? print-build-trace?
                         #:max-silent-time max-silent-time
                         #:build-timeout build-timeout))))
                   (else
                    (leave (G_ "invalid request line: ~s~%") line)))
             (loop (read-line)))))))
    (("test" rest ...)
     (with-error-handling
       (let-values (((file pred)
                     (match rest
                       ((file regexp)
                        (values file
                                (compose (cut string-match regexp <>)
                                         build-machine-name)))
                       ((file) (values file (const #t)))
                       (()     (values %machine-file (const #t)))
                       (x      (leave (G_ "wrong number of arguments~%"))))))
         (check-machines-availability-from-file (or file %machine-file)
                                                pred))))
    (("status" rest ...)
     (with-error-handling
       (let-values (((file pred)
                     (match rest
                       ((file regexp)
                        (values file
                                (compose (cut string-match regexp <>)
                                         build-machine-name)))
                       ((file) (values file (const #t)))
                       (()     (values %machine-file (const #t)))
                       (x      (leave (G_ "wrong number of arguments~%"))))))
         (check-machine-status (or file %machine-file) pred))))
    (("--version")
     (show-version-and-exit "guix offload"))
    (("--help")
     (leave-on-EPIPE
      (format #t (G_ "Usage: guix offload SYSTEM MAX-SILENT-TIME \
PRINT-BUILD-TRACE? BUILD-TIMEOUT
Process build offload requests written on the standard input, possibly
offloading builds to the machines listed in '~a'.~%")
              %machine-file))
     (display (G_ "
This tool is meant to be used internally by 'guix-daemon'.\n"))
     (show-bug-report-information))
    (x
     (leave (G_ "invalid arguments: ~{~s ~}~%") x))))

;;; Local Variables:
;;; eval: (put 'with-timeout 'scheme-indent-function 2)
;;; End:

;;; offload.scm ends here
