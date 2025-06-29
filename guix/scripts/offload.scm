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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
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

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix offload")))))

(define %default-options
  ;; Alist of default option values.
  '())

(define (parse-args args)

  (define actions '("status" "test"))

  (define (parse-file+regexp arg result)
    (cond ((assoc-ref result 'regexp)
           (leave (G_ "~A: extraneous argument~%") arg))
          ((assoc-ref result 'file)
           (alist-cons 'regexp arg result))
          (else
           (alist-cons 'file arg result))))

  (define (handle-argument arg result)
    (cond ((member arg actions)
           (alist-cons 'action (string->symbol arg) result))
          ((member (assoc-ref result 'action) '(status test))
           (parse-file+regexp arg result))
          ((> (length result) 3)
           (leave (G_ "~A: extraneous argument~%") arg))
          (else
           (cons arg result))))

  (let ((result (parse-command-line args
                                    %options
                                    (list %default-options)
                                    #:argument-handler handle-argument)))
    (if (assoc-ref result 'action)
        ;; action alist
        result
        ;; default: SYSTEM MAX-SILENT-TIME PRINT-BUILD-TRACE? BUILD-TIMEOUT
        (reverse result))))

(define (make-build-request-processor system
                                      max-silent-time
                                      print-build-trace?
                                      build-timeout)
  "Returns a function that processes a line with process-build-request with
settings SYSTEM, MAX-SILENT-TIME, PRINT-BUILD-TRACE? and BUILD-TIMEOUT."
  (let ((not-coma (char-set-complement (char-set #\,)))
        (max-silent-time    (string->number max-silent-time))
        (build-timeout      (string->number build-timeout))
        (print-build-trace? (string=? print-build-trace? "1")))
    (set-thread-name "guix offload")
    (parameterize ((%current-system system))
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
           #:build-timeout build-timeout))))))

(define (show-help)
  (display (G_ "Usage: guix offload [COMMAND] [OPTIONS...]
Inspect remote build machines for offloading package builds.~%"))
  (display (G_ "
COMMAND must be one of the sub-commands listed below:

  test [FILE [REGEXP]]     test availability and set up of build machines
  status [FILE [REGEXP]]   display the load and availability of build machines~%"))
  (display (G_ "

The build machines are read from '~a' by default, or from FILE when specified.
When REGEXP is provided, only machines whose names match REGEXP are considered.\n")
           %machine-file)
  (display (G_ "

When invoked without a sub-command, 'guix offload' processes build offload
requests written on standard input. This mode is meant to be used internally
by 'guix-daemon' as a build hook:
guix offload SYSTEM MAX-SILENT-TIME PRINT-BUILD-TRACE? BUILD-TIMEOUT~%"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (display (G_ "
The machine configuration file should evaluate to a list of <build-machine>
records.  See 'info \"(guix) Daemon Offload Setup\"' for details.\n"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define-command (guix-offload . args)
  (category plumbing)
  (synopsis "set up and operate build offloading")

  (define request-line-rx
    ;; The request format.  See 'tryBuildHook' method in build.cc.
    (make-regexp "([01]) ([a-z0-9_-]+) (/[[:graph:]]+.drv) ([[:graph:]]*)"))

  ;; Make sure $HOME really corresponds to the current user.  This is
  ;; necessary since lsh uses that to determine the location of the yarrow
  ;; seed file, and fails if it's owned by someone else.
  (and=> (passwd:dir (getpw (getuid)))
         (cut setenv "HOME" <>))

  (define opts (parse-args args))

  (match (assoc-ref opts 'action)
    ('test
      (with-error-handling
        (let ((regexp (assoc-ref opts 'regexp)))
          (check-machines-availability-from-file
           (or (assoc-ref opts 'file) %machine-file)
           (if regexp
               (compose (cut string-match regexp <>) build-machine-name)
               (const #t))))))
    ('status
      (with-error-handling
        (let ((regexp (assoc-ref opts 'regexp)))
          (check-machine-status
           (or (assoc-ref opts 'file) %machine-file)
           (if regexp
               (compose (cut string-match regexp <>) build-machine-name)
               (const #t))))))
    ((system max-silent-time print-build-trace? build-timeout)
     (let ((process-line (make-build-request-processor system
                                                       max-silent-time
                                                       print-build-trace?
                                                       build-timeout)))
       (let loop ((line (read-line)))
         (unless (eof-object? line)
           (cond ((regexp-exec request-line-rx line)
                  => process-line)
                 (else
                  (leave (G_ "invalid request line: ~s~%") line)))
           (loop (read-line))))))
    (x
     (leave (G_ "invalid parsed arguments: ~{~s ~}~%") x))))

;;; Local Variables:
;;; eval: (put 'with-timeout 'scheme-indent-function 2)
;;; End:

;;; offload.scm ends here
