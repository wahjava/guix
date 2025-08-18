;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 firefly707 <firejet707@gmail.com>
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

(define-module (gnu home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:autoload   (gnu packages emacs) (emacs)
  #:use-module (guix records)
  #:use-module (guix gexp)
  
  #:export (emacs-server-configuration
            emacs-server-service-type))

(define-record-type* <emacs-server-configuration>
  emacs-server-configuration make-emacs-server-configuration
  emacs-server-configuration?
  (emacs emacs-server-emacs
         (default emacs)
         (documentation "The @code{emacs} package to use."))
  (args emacs-server-args
        (default '())
        (documentation "Extra arguments to pass to @code{emacs}."))
  (server-name emacs-server-server-name
               (default #f)
               (documentation "\
The name of the @code{emacs} server to open.  By default, doesn't specify a name,
 and uses whatever default name @code{emacs} chooses."))
  (service-name emacs-server-service-name
                (default 'emacs-server)
                (documentation "\
The name to give the shepherd service, for use in command like @code{herd status}.
  By default, emacs-server.  This is  useful if you want to run multiple servers,
 for example if one is a computational server.")))

(define (emacs-server-shepherd-service config)
  (match-record config <emacs-server-configuration>
                (emacs args server-name service-name)
    (list
     (shepherd-service
       (provision (list service-name))
       (modules '((shepherd support)))
       (start #~(make-forkexec-constructor
                 '(#$(file-append
                      emacs
                      "/bin/emacs")
                   #$(string-append
                      "--fg-daemon"
                      (if
                       server-name
                       (string-append "=" server-name)
                       ""))
                   #$@args)
                 #:log-file
                 (string-append
                  %user-log-dir
                  #$(string-append "/"
                                   (symbol->string service-name)
                                   ".log"))))
       (stop #~(make-kill-destructor))))))

(define (add-emacs-packages config)
  (list (emacs-server-emacs config)))

(define emacs-server-service-type
  (service-type
    (name 'emacs-server)
    (extensions
     (list
      (service-extension home-shepherd-service-type
                         emacs-server-shepherd-service)
      (service-extension home-profile-service-type
                         add-emacs-packages)))
    (default-value (emacs-server-configuration))
    (description "Run @code{emacs} as a daemon on user login, and provide emacs in
 the home profile.")))
