;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015-2016, 2022-2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Aljosha Papsch <ep@stern-data.com>
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services databases)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services configuration environment-variables)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:autoload   (gnu system accounts) (default-shell)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (guix build-system trivial)
  #:use-module (guix build union)
  #:use-module (guix deprecation)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (postgresql-config-file
            postgresql-config-file?
            postgresql-config-file-log-destination
            postgresql-config-file-hba-file
            postgresql-config-file-ident-file
            postgresql-config-file-socket-directory
            postgresql-config-file-extra-config

            postgresql-configuration
            postgresql-configuration?
            postgresql-configuration-allow-login?
            postgresql-configuration-create-account?
            postgresql-configuration-data-directory
            postgresql-configuration-extension-packages
            postgresql-configuration-file
            postgresql-configuration-gid
            postgresql-configuration-home-directory
            postgresql-configuration-locale
            postgresql-configuration-log-directory
            postgresql-configuration-port
            postgresql-configuration-postgresql
            postgresql-configuration-uid

            postgresql-service
            postgresql-service-type

            %default-postgresql-role-shepherd-requirement

            postgresql-role
            postgresql-role?
            postgresql-role-name
            postgresql-role-password-file
            postgresql-role-permissions
            postgresql-role-create-database?
            postgresql-role-configuration
            postgresql-role-configuration?
            postgresql-role-configuration-host
            postgresql-role-configuration-shepherd-requirement
            postgresql-role-configuration-roles

            postgresql-role-service-type

            postgresql-backup-configuration
            postgresql-backup-configuration?
            postgresql-backup-configuration-fields
            postgresql-backup-configuration-package
            postgresql-backup-configuration-schedule
            postgresql-backup-configuration-shepherd-requirement
            postgresql-backup-configuration-databases
            postgresql-backup-configuration-backup-user
            postgresql-backup-configuration-hostname
            postgresql-backup-configuration-username
            postgresql-backup-configuration-backup-dir
            postgresql-backup-configuration-schema-only-list
            postgresql-backup-configuration-enable-custom-backups?
            postgresql-backup-configuration-enable-plain-backups?
            postgresql-backup-configuration-enable-globals-backups
            postgresql-backup-configuration-day-of-week-to-keep
            postgresql-backup-configuration-days-to-keep
            postgresql-backup-configuration-weeks-to-keep
            postgresql-backup-configuration-file
            postgresql-backup-log-file
            postgresql-backup-program

            postgresql-backup-extension
            postgresql-backup-extension?
            postgresql-backup-extension-fields
            postgresql-backup-extension-databases

            postgresql-backup-service-type

            memcached-service-type
            memcached-configuration
            memcached-configuration?
            memcached-configuration-memecached
            memcached-configuration-interfaces
            memcached-configuration-tcp-port
            memcached-configuration-udp-port
            memcached-configuration-additional-options

            mysql-service
            mysql-service-type
            mysql-configuration
            mysql-configuration?

            redis-configuration
            redis-configuration?
            redis-service-type))

;;; Commentary:
;;;
;;; Database services.
;;;
;;; Code:

(define %default-postgres-hba
  (plain-file "pg_hba.conf"
              "
local	all	all			peer
host	all	all	127.0.0.1/32 	md5
host	all	all	::1/128 	md5"))

(define %default-postgres-ident
  (plain-file "pg_ident.conf"
              "# MAPNAME       SYSTEM-USERNAME         PG-USERNAME"))

(define-record-type* <postgresql-config-file>
  postgresql-config-file make-postgresql-config-file
  postgresql-config-file?
  (log-destination   postgresql-config-file-log-destination
                     (default "syslog"))
  (hba-file          postgresql-config-file-hba-file
                     (default %default-postgres-hba))
  (ident-file        postgresql-config-file-ident-file
                     (default %default-postgres-ident))
  (socket-directory  postgresql-config-file-socket-directory
                     (default "/var/run/postgresql"))
  (extra-config      postgresql-config-file-extra-config
                     (default '())))

(define-gexp-compiler (postgresql-config-file-compiler
                       (file <postgresql-config-file>) system target)
  (match file
    (($ <postgresql-config-file> log-destination hba-file
                                 ident-file socket-directory
                                 extra-config)
     ;; See: https://www.postgresql.org/docs/current/config-setting.html.
    (define (format-value value)
      (cond
       ((boolean? value)
        (list (if value "on" "off")))
       ((number? value)
        (list (number->string value)))
       (else
        (list "'" value "'"))))

    (define contents
      (append-map
       (match-lambda
         ((key) '())
         ((key . #f) '())
         ((key values ...)
          `(,key " = " ,@(append-map format-value values) "\n")))

       `(("log_destination" ,log-destination)
         ("hba_file" ,hba-file)
         ("ident_file" ,ident-file)
         ,@(if socket-directory
               `(("unix_socket_directories" ,socket-directory))
               '())
         ,@extra-config)))

     (gexp->derivation
      "postgresql.conf"
      #~(call-with-output-file (ungexp output "out")
          (lambda (port)
            (display
             (string-append #$@contents)
             port)))
      #:local-build? #t))))

(define %default-home-directory "/var/empty")

(define-record-type* <postgresql-configuration>
  postgresql-configuration make-postgresql-configuration
  postgresql-configuration?
  ;; Setting no default ensures that the user sets its current postgresql
  ;; explicitely.  Since major upgrades currently require a manual migration
  ;; of the database, this way the user is responsible for upgrading properly.
  ;; MAYBE TODO: Write an action to automatically upgrade in Guix.
  (postgresql         postgresql-configuration-postgresql) ;file-like
  (port               postgresql-configuration-port
                      (default 5432))
  (locale             postgresql-configuration-locale
                      (default "en_US.utf8"))
  (config-file        postgresql-configuration-file
                      (default (postgresql-config-file)))
  (log-directory      postgresql-configuration-log-directory
                      (default "/var/log/postgresql"))
  (data-directory     postgresql-configuration-data-directory
                      (default "/var/lib/postgresql/data"))
  (extension-packages postgresql-configuration-extension-packages
                      (default '()))
  (create-account?    postgresql-configuration-create-account?
                      (default #t))
  (home-directory     postgresql-configuration-home-directory
                      (default %default-home-directory))
  (allow-login?       postgresql-configuration-allow-login?
                      (default #f))
  (uid                postgresql-configuration-uid
                      (default #f))
  (gid                postgresql-configuration-gid
                      (default #f)))

(define (create-postgresql-account config)
  (match-record config <postgresql-configuration>
                (create-account? allow-login? home-directory uid gid)
    (if (not create-account?) '()
        (list (user-group
               (name "postgres")
               (id gid)
               (system? #t))
              (user-account
               (name "postgres")
               (group "postgres")
               (system? #t)
               (uid uid)
               (comment "PostgreSQL server user")
               (create-home-directory?
                (not (string=? home-directory %default-home-directory)))
               (home-directory home-directory)
               (shell (if allow-login?
                          (default-shell)
                          (file-append shadow "/sbin/nologin"))))))))

(define (final-postgresql postgresql extension-packages)
  (if (null? extension-packages)
    postgresql
    (package
      (inherit postgresql)
      (source #f)
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils) (guix build union))
         #:builder
         (begin
           (use-modules (guix build utils) (guix build union) (srfi srfi-26))
           (union-build (assoc-ref %outputs "out")
                        (map (lambda (input) (cdr input))
                             %build-inputs))
           #t)))
      (inputs
       `(("postgresql" ,postgresql)
         ,@(map (lambda (extension) (list "extension" extension))
                extension-packages))))))

(define postgresql-activation
  (match-lambda
    (($ <postgresql-configuration> postgresql port locale config-file
                                   log-directory data-directory
                                   extension-packages)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "postgres")))
           ;; Create the socket directory.
           (let ((socket-directory
                  #$(postgresql-config-file-socket-directory config-file)))
             (when (string? socket-directory)
               (mkdir-p socket-directory)
               (chown socket-directory (passwd:uid user) (passwd:gid user))))

           ;; Create the log directory.
           (when (string? #$log-directory)
             (mkdir-p #$log-directory)
             (chown #$log-directory (passwd:uid user) (passwd:gid user)))

           (unless (file-exists? #$data-directory)
             (let ((initdb (string-append
                            #$(final-postgresql postgresql
                                                extension-packages)
                            "/bin/initdb"))
                   (initdb-args
                    (append
                     (if #$locale
                         (list (string-append "--locale=" #$locale))
                         '()))))
               ;; Create db state directory.
               (mkdir-p #$data-directory)
               (chown #$data-directory (passwd:uid user) (passwd:gid user))

               ;; Drop privileges and init state directory in a new
               ;; process.  Wait for it to finish before proceeding.
               (match (primitive-fork)
                 (0
                  ;; Exit with a non-zero status code if an exception is
                  ;; thrown.
                  (dynamic-wind
                      (const #t)
                      (lambda ()
                        (setgid (passwd:gid user))
                        (setuid (passwd:uid user))
                        (apply execl initdb
                               initdb "-D" #$data-directory
                               initdb-args))
                      (lambda ()
                        (primitive-exit 1))))
                 (pid (waitpid pid))))))))))

(define postgresql-shepherd-service
  (match-lambda
    (($ <postgresql-configuration> postgresql port locale config-file
                                   log-directory data-directory
                                   extension-packages)
     (let* ((pg_ctl-wrapper
             ;; Wrapper script that switches to the 'postgres' user before
             ;; launching daemon.
             (program-file
              "pg_ctl-wrapper"
              #~(begin
                  (use-modules (ice-9 match)
                               (ice-9 format))
                  (match (command-line)
                    ((_ mode)
                     (let ((user (getpwnam "postgres"))
                           (pg_ctl #$(file-append
                                      (final-postgresql postgresql
                                                        extension-packages)
                                                  "/bin/pg_ctl"))
                           (options (format #f "--config-file=~a -p ~d"
                                            #$config-file #$port)))
                       (setgid (passwd:gid user))
                       (setuid (passwd:uid user))
                       (execl pg_ctl pg_ctl "-D" #$data-directory
                              #$@(if (string? log-directory)
                                     (list "-l"
                                           (string-append log-directory
                                                          "/pg_ctl.log"))
                                     '())
                              "-o" options
                              mode)))))))
            (pid-file (in-vicinity data-directory "postmaster.pid"))
            (action (lambda args
                      #~(lambda _
                          (invoke #$pg_ctl-wrapper #$@args)
                          (match '#$args
                            (("start")
                             (call-with-input-file #$pid-file read))
                            (_ #t))))))
       (list (shepherd-service
              (provision '(postgres postgresql))
              (documentation "Run the PostgreSQL daemon.")
              (requirement '(user-processes loopback syslogd))
              (modules `((ice-9 match)
                         ,@%default-modules))
              (actions (list (shepherd-configuration-action config-file)))
              (start (action "start"))
              (stop (action "stop"))))))))

(define postgresql-service-type
  (service-type
   (name 'postgresql)
   (extensions
    (list (service-extension shepherd-root-service-type
                             postgresql-shepherd-service)
          (service-extension activation-service-type
                             postgresql-activation)
          (service-extension account-service-type
                             create-postgresql-account)
          (service-extension
           profile-service-type
           (compose list postgresql-configuration-postgresql))))
   (description "Run the PostgreSQL database server.")))

(define-deprecated (postgresql-service #:key (postgresql postgresql-10)
                                       (port 5432)
                                       (locale "en_US.utf8")
                                       (config-file (postgresql-config-file))
                                       (data-directory
                                        "/var/lib/postgresql/data")
                                       (extension-packages '()))
  postgresql-service-type
  "Return a service that runs @var{postgresql}, the PostgreSQL database
server.

The PostgreSQL daemon loads its runtime configuration from @var{config-file}
and stores the database cluster in @var{data-directory}."
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql)
            (port port)
            (locale locale)
            (config-file config-file)
            (data-directory data-directory)
            (extension-packages extension-packages))))

(define-record-type* <postgresql-role>
  postgresql-role make-postgresql-role
  postgresql-role?
  (name             postgresql-role-name) ;string
  (password-file    postgresql-role-password-file  ;string
                    (default #f))
  (permissions      postgresql-role-permissions
                    (default '(createdb login))) ;list
  (create-database? postgresql-role-create-database?  ;boolean
                    (default #f))
  (encoding postgresql-role-encoding ;string
            (default "UTF8"))
  (collation postgresql-role-collation ;string
             (default "en_US.utf8"))
  (ctype postgresql-role-ctype ;string
         (default "en_US.utf8"))
  (template postgresql-role-template ;string
            (default "template1")))

(define %default-postgresql-role-shepherd-requirement
  '(user-processes postgres))

(define-record-type* <postgresql-role-configuration>
  postgresql-role-configuration make-postgresql-role-configuration
  postgresql-role-configuration?
  (shepherd-requirement
   postgresql-role-configuration-shepherd-requirement ;list-of-symbols
   (default %default-postgresql-role-shepherd-requirement))
  (host             postgresql-role-configuration-host ;string
                    (default "/var/run/postgresql"))
  (log              postgresql-role-configuration-log ;string
                    (default "/var/log/postgresql_roles.log"))
  (roles            postgresql-role-configuration-roles
                    (default '()))) ;list

(define (postgresql-create-roles config)
  ;; See: https://www.postgresql.org/docs/current/sql-createrole.html for the
  ;; complete permissions list.
  (define (format-permissions permissions)
    (let ((dict '(bypassrls createdb createrole login replication superuser)))
      (string-join (filter-map (lambda (permission)
                                 (and (member permission dict)
                                      (string-upcase
                                       (symbol->string permission))))
                               permissions)
                   " ")))

  (define (password-value role)
    (string-append "password_" (postgresql-role-name role)))

  (define (role->password-variable role)
    (let ((file-name (postgresql-role-password-file role)))
      (if (string? file-name)
          ;; This way passwords do not leak to the command line.
          #~(string-append "-v \"" #$(password-value role)
                           "=$(" #$coreutils "/bin/cat " #$file-name ")\"")
          "")))

  (define (roles->queries roles)
    (apply mixed-text-file "queries"
           (append-map
            (lambda (role)
              (match-record role <postgresql-role>
                (name permissions create-database? encoding collation ctype
                      template password-file)
                `("SELECT NOT(EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE \
rolname = '" ,name "')) as not_exists;\n"
"\\gset\n"
"\\if :not_exists\n"
"CREATE ROLE \"" ,name "\""
" WITH " ,(format-permissions permissions)
,(if (and (string? password-file)
          (not (string-null? password-file)))
     (string-append
      "\nPASSWORD :'" (password-value role) "'")
     "")
";\n"
,@(if create-database?
      `("CREATE DATABASE \"" ,name "\""
        " OWNER \"" ,name "\"\n"
        " ENCODING '" ,encoding "'\n"
        " LC_COLLATE '" ,collation "'\n"
        " LC_CTYPE '" ,ctype "'\n"
        " TEMPLATE " ,template ";")
      '())
"\\endif\n")))
            roles)))

  (let ((host (postgresql-role-configuration-host config))
        (roles (postgresql-role-configuration-roles config)))
    (program-file "run-queries"
      #~(let ((bash #$(file-append bash-minimal "/bin/bash"))
              (psql #$(file-append postgresql "/bin/psql")))
          (define command
            (string-append
             "set -e; exec " psql " -a -h " #$host " -f "
             #$(roles->queries roles) " "
             (string-join
              (list
               #$@(map role->password-variable roles))
              " ")))
          (execlp bash bash "-c" command)))))

(define (postgresql-role-shepherd-service config)
  (match-record config <postgresql-role-configuration>
    (log shepherd-requirement)
    (list (shepherd-service
           (requirement shepherd-requirement)
           (provision '(postgres-roles))
           (one-shot? #t)
           (start
            #~(lambda args
                (zero? (spawn-command
                        (list #$(postgresql-create-roles config))
                        #:user "postgres"
                        #:group "postgres"
                        ;; XXX: As of Shepherd 1.0.2, #:log-file is not
                        ;; supported.
                        ;; #:log-file #$log
                        ))))
           (documentation "Create PostgreSQL roles.")))))

(define postgresql-role-service-type
  (service-type (name 'postgresql-role)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          postgresql-role-shepherd-service)))
                (compose concatenate)
                (extend (lambda (config extended-roles)
                          (match-record config <postgresql-role-configuration>
                            (host roles)
                            (postgresql-role-configuration
                             (inherit config)
                             (host host)
                             (roles (append roles extended-roles))))))
                (default-value (postgresql-role-configuration))
                (description "Ensure the specified PostgreSQL roles are
created after the PostgreSQL database is started.")))

(define (gexp-or-string? value)
  (or (gexp? value)
      (string? value)))

(define list-of-strings?
  (list-of string?))
(define list-of-symbols?
  (list-of symbol?))

(define-maybe/no-serialization string)
(define-maybe/no-serialization positive)

(define-configuration/no-serialization postgresql-backup-configuration
  (package
   (package postgresql-backup-scripts)
   "The @code{postgresql-backup-scripts} used to run backups.")
  (schedule
   (gexp-or-string)
   "A string or a gexp representing the frequency of the dumps.  Gexp must
evaluate to @code{calendar-event} records or to strings.  Strings must contain
Vixie cron date lines.")
  (shepherd-requirement
   (list-of-symbols '(user-processes file-systems postgresql))
   "A list of Shepherd services that will be waited for, before starting
the backup service.")
  (databases
   (list-of-strings '())
   "List of database names.  These will be the only databases to be dumped.
When unset all databases in the cluster will be dumped (with the exclusion of
those mentioned in the @code{schema-only-list} field, which will have only their
schema dumped).")
  (backup-user
   (maybe-string)
   "Optional system user to run backups as.  If the user the script is running
as doesn't match this the script terminates.  Leave blank to skip check.")
  (hostname
   (string "/var/run/postgresql")
   "Optional hostname to adhere to @code{pg_hba} policies.")
  (username
   (string "postgres")
   "Optional username to connect to database as.")
  (backup-dir
   (string "/var/lib/postgresql-backups")
   "The directory where backups will be stored.")
  (schema-only-list
   (list-of-strings '())
   "List of strings to match against in database name, separated by space or
comma, for which we only wish to keep a backup of the schema, not the data.
Any database names which contain any of these values will be considered
candidates (e.g. @code{\"system_log\"} will match
@code{\"dev_system_log_2010-01\"}).")
  (enable-custom-backups?
   (boolean #t)
   "Whether the service will produce a custom-format backup.")
  (enable-plain-backups?
   (boolean #f)
   "Whether the service will produce a gzipped plain-format backup.")
  (enable-globals-backups?
   (boolean #f)
   "Whether the service will produce gzipped SQL file containing the cluster
globals, like users and passwords.")
  (day-of-week-to-keep
   (maybe-positive)
   "Which day to take the weekly backup from (1-7 = Monday-Sunday).")
  (days-to-keep
   (maybe-positive)
   "Number of days to keep daily backups.")
  (weeks-to-keep
   (maybe-positive)
   "How many weeks to keep weekly backups."))

(define-configuration/no-serialization postgresql-backup-extension
  (databases
   (list-of-strings '())
   "List of database names.  These will be the merged with
names from other extensions."))

(define (serialize-postgresql-backup-configuration config)
  (define databases
    (postgresql-backup-configuration-databases config))
  (define schema-only-list
    (postgresql-backup-configuration-schema-only-list config))
  (define variables
    (configuration->environment-variables
     config postgresql-backup-configuration-fields
     #:excluded '(schedule schema-only-list
                  shepherd-requirement package databases)
     #:true-value "yes"
     #:false-value "no"))
  #~(begin
      (use-modules (ice-9 format))
      (let ((schema-only-list (list #$@schema-only-list))
            (databases (list #$@databases))
            (variables
             (list #$@(map (lambda (variable)
                             #~(format #f "~a=~a"
                                       #$(car variable)
                                       #$(cdr variable)))
                           variables))))
        (format #f "##############################
## POSTGRESQL BACKUP CONFIG ##
##############################
~{~%~a~}

######################################~%"
                (append
                 (if (> (length schema-only-list) 0)
                     (list
                      (format #f "SCHEMA_ONLY_LIST=\"~{ ~a~}\""
                              schema-only-list))
                     '())
                 (if (> (length databases) 0)
                     (list
                      (format #f "DATABASES=\"~{ ~a~}\""
                              databases))
                     '())
                 variables)))))

(define (postgresql-backup-configuration-file config)
  (mixed-text-file "pg_backup.config"
   (serialize-postgresql-backup-configuration config)))

(define (postgresql-backup-user config)
  (define backup-user (postgresql-backup-configuration-username config))
  (if (maybe-value-set? backup-user)
      backup-user
      (postgresql-backup-configuration-username config)))

(define (rotated-backup? config)
  (any
   maybe-value-set?
   (list
    (postgresql-backup-configuration-day-of-week-to-keep config)
    (postgresql-backup-configuration-days-to-keep config)
    (postgresql-backup-configuration-weeks-to-keep config))))

(define (postgresql-backup-entrypoint config)
  #~(string-append
     #$(postgresql-backup-configuration-package config)
     (if #$(rotated-backup? config)
         "/bin/pg_backup_rotated.sh"
         "/bin/pg_backup.sh")))

(define (postgresql-backup-program config)
  (program-file "postgresql-backup"
   #~(begin
       (let ((bash (string-append #$bash-minimal "/bin/bash"))
             (configuration-file
              #$(postgresql-backup-configuration-file config)))
         (execlp bash bash #$(postgresql-backup-entrypoint config)
                 "-c" configuration-file)))))

(define (postgresql-backup-log-file config)
  "/var/log/postgresql-backup.log")

(define (postgresql-backup-configuration->shepherd-service config)
  (let ((schedule (postgresql-backup-configuration-schedule config))
        (configuration-file
         (postgresql-backup-configuration-file config))
        (user (postgresql-backup-user config))
        (log-file (postgresql-backup-log-file config))
        (shepherd-requirement
         (postgresql-backup-configuration-shepherd-requirement config)))
    (shepherd-service (provision '(postgres-backups))
                      (requirement shepherd-requirement)
                      (modules
                       '((shepherd service timer)))
                      (documentation
                       "Run PostgreSQL backups on a regular basis.")
                      (start
                       #~(make-timer-constructor
                          (if (string? #$schedule)
                              (cron-string->calendar-event #$schedule)
                              #$schedule)
                          (command
                           (list #$(postgresql-backup-program config))
                           #:user #$user
                           #:group
                           (group:name
                            (getgrgid
                             (passwd:gid (getpwnam #$user)))))
                          #:log-file #$log-file
                          #:wait-for-termination? #t))
                      (stop #~(make-timer-destructor))
                      (actions
                       (list (shepherd-action
                              (inherit shepherd-trigger-action)
                              (documentation "Manually trigger a dump,
without waiting for the scheduled time."))
                             (shepherd-action
                              (name 'configuration)
                              (documentation
                               "Prints the configuration file name.")
                              (procedure
                               #~(lambda _
                                   (format #t "~a~%"
                                           #$configuration-file)))))))))

(define (postgresql-backup-activation config)
  "Return an activation gexp for Postgresql-Backup."
  #~(begin
      (use-modules (guix build utils))
      (let* ((user (getpwnam #$(postgresql-backup-user config)))
             (uid (passwd:uid user))
             (gid (passwd:gid user))
             (dir #$(postgresql-backup-configuration-backup-dir config)))
        ;; Setup datadir
        (mkdir-p dir)
        (chown dir uid gid)
        (chmod dir #o700))))

(define (postgresql-backup-databases-merge a b)
  (let loop ((merged '())
             (lst (append a b)))
    (if (null? lst)
        merged
        (loop
         (let ((element (car lst)))
           (when (member element merged)
             (raise
              (formatted-message
               (G_ "Duplicated database name: ~a. Database names should be
unique, please remove the duplicate.") element)))
           (cons element merged))
         (cdr lst)))))

(define (postgresql-backup-extension-merge a b)
  (postgresql-backup-extension
   (databases (postgresql-backup-databases-merge
                (postgresql-backup-extension-databases a)
                (postgresql-backup-extension-databases b)))))

(define postgresql-backup-service-type
  (service-type
   (name 'postgresql-backup)
   (extensions
    (list
     (service-extension
      activation-service-type
      postgresql-backup-activation)
     (service-extension
      profile-service-type
      (compose list postgresql-backup-configuration-package))
     (service-extension
      shepherd-root-service-type
      (compose
       list postgresql-backup-configuration->shepherd-service))))
   ;; Concatenate POSTGRESQL-BACKUP databases names.
   (compose (lambda (args)
              (fold postgresql-backup-extension-merge
                    (postgresql-backup-extension)
                    args)))
   (extend
    (lambda (config extension)
      (postgresql-backup-configuration
       (inherit config)
       (databases
        (postgresql-backup-databases-merge
         (postgresql-backup-configuration-databases config)
         (postgresql-backup-extension-databases extension))))))
   (description
    "Run PostgreSQL database dumps on a regular basis.")))


;;;
;;; Memcached
;;;

(define-record-type* <memcached-configuration>
  memcached-configuration make-memcached-configuration
  memcached-configuration?
  (memcached          memcached-configuration-memcached ;file-like
                      (default memcached))
  (interfaces         memcached-configuration-interfaces
                      (default '("0.0.0.0")))
  (tcp-port           memcached-configuration-tcp-port
                      (default 11211))
  (udp-port           memcached-configuration-udp-port
                      (default 11211))
  (additional-options memcached-configuration-additional-options
                      (default '())))

(define %memcached-accounts
  (list (user-group (name "memcached") (system? #t))
        (user-account
         (name "memcached")
         (group "memcached")
         (system? #t)
         (comment "Memcached server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define memcached-activation
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "memcached")))
        (mkdir-p "/var/run/memcached")
        (chown "/var/run/memcached"
               (passwd:uid user) (passwd:gid user)))))

(define memcached-shepherd-service
  (match-lambda
    (($ <memcached-configuration> memcached interfaces tcp-port udp-port
                                  additional-options)
     (list (shepherd-service
            (provision '(memcached))
            (documentation "Run the Memcached daemon.")
            (requirement '(user-processes loopback))
            (start #~(make-forkexec-constructor
                      `(#$(file-append memcached "/bin/memcached")
                        "-l" #$(string-join interfaces ",")
                        "-p" #$(number->string tcp-port)
                        "-U" #$(number->string udp-port)
                        "--daemon"
                        ;; Memcached changes to the memcached user prior to
                        ;; writing the pid file, so write it to a directory
                        ;; that memcached owns.
                        "-P" "/var/run/memcached/pid"
                        "-u" "memcached"
                        ,#$@additional-options)
                      #:log-file "/var/log/memcached"
                      #:pid-file "/var/run/memcached/pid"))
            (stop #~(make-kill-destructor)))))))

(define memcached-service-type
  (service-type (name 'memcached)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          memcached-shepherd-service)
                       (service-extension activation-service-type
                                          (const memcached-activation))
                       (service-extension account-service-type
                                          (const %memcached-accounts))))
                (default-value (memcached-configuration))
                (description "Run @command{memcached}, a daemon that provides
an in-memory caching service, intended for use by dynamic web
applications.")))


;;;
;;; MySQL.
;;;

(define-record-type* <mysql-configuration>
  mysql-configuration make-mysql-configuration
  mysql-configuration?
  (mysql mysql-configuration-mysql (default mariadb))
  (bind-address mysql-configuration-bind-address (default "127.0.0.1"))
  (port mysql-configuration-port (default 3306))
  (socket mysql-configuration-socket (default "/run/mysqld/mysqld.sock"))
  (datadir mysql-configuration-datadir (default "/var/lib/mysql"))
  (extra-content mysql-configuration-extra-content (default ""))
  (extra-environment mysql-configuration-extra-environment (default #~'()))
  (auto-upgrade? mysql-configuration-auto-upgrade? (default #t)))

(define %mysql-accounts
  (list (user-group
         (name "mysql")
         (system? #t))
        (user-account
         (name "mysql")
         (group "mysql")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define mysql-configuration-file
  (match-lambda
    (($ <mysql-configuration> mysql bind-address port socket datadir extra-content)
     (mixed-text-file "my.cnf" "[mysqld]
datadir=" datadir "
socket=" socket "
bind-address=" bind-address "
port=" (number->string port) "
" extra-content "
"))))

(define (mysqld-wrapper config)
  "Start mysqld, and initialize the system tables if necessary."
  (program-file
   "mysqld-wrapper"
   (with-imported-modules (source-module-closure
                           '((guix build utils)))
     (let ((mysql     (mysql-configuration-mysql config))
           (datadir   (mysql-configuration-datadir config))
           (my.cnf    (mysql-configuration-file config)))
       #~(begin
           (use-modules (guix build utils))
           (let* ((mysqld (string-append #$mysql "/bin/mysqld"))
                  (user    (getpwnam "mysql"))
                  (uid     (passwd:uid user))
                  (gid     (passwd:gid user))
                  (rundir  "/run/mysqld"))
             (mkdir-p #$datadir)
             (chown #$datadir uid gid)
             (mkdir-p rundir)
             (chown rundir uid gid)
             (unless (file-exists? (string-append #$datadir "/mysql"))
               (let ((init (system* #$(mysql-install config))))
                 (unless (= 0 (status:exit-val init))
                   (throw 'system-error "MySQL initialization failed."))))
             ;; Drop privileges and start the server.
             (setgid gid) (setuid uid)
             (execl mysqld mysqld
                    (string-append "--defaults-file=" #$my.cnf))))))))

(define (mysql-shepherd-service config)
  (list (shepherd-service
         (provision '(mysql))
         (requirement '(user-processes))
         (documentation "Run the MySQL server.")
         (actions (list (shepherd-configuration-action
                         (mysql-configuration-file config))))
         (start (let ((mysql (mysql-configuration-mysql config))
                      (extra-env (mysql-configuration-extra-environment config))
                      (my.cnf (mysql-configuration-file config)))
                  #~(make-forkexec-constructor
                     (list #$(mysqld-wrapper config))
                     #:log-file "/var/log/mysqld.log"
                     #:environment-variables #$extra-env)))
         (stop #~(make-kill-destructor)))))

(define (mysql-install config)
  "Install MySQL system database and secure the installation."
  (let ((mysql   (mysql-configuration-mysql config))
        (my.cnf  (mysql-configuration-file config)))
    (program-file
     "mysql-install"
     (with-imported-modules (source-module-closure
                             '((guix build utils)))
       #~(begin
           (use-modules (guix build utils))
           ;; Make sed, mkdir, uname, etc available for mariadb-install-db.
           (set-path-environment-variable "PATH" '("bin")
                                          (list #$sed #$coreutils))
           (if (string=? "mariadb" #$(package-name mysql))
               ;; For MariaDB.
               (system* #$(file-append mysql "/bin/mariadb-install-db")
                        (string-append "--defaults-file=" #$my.cnf)
                        "--skip-test-db"
                        "--user=mysql")
               ;; For MySQL.
               (system* #$(file-append mysql "/bin/mysqld")
                        (string-append "--defaults-file=" #$my.cnf)
                        "--initialize"
                        "--user=mysql")))))))

(define (mysql-upgrade-wrapper config)
  ;; The MySQL socket and PID file may appear before the server is ready to
  ;; accept connections.  Ensure the socket is responsive before attempting
  ;; to run the upgrade script.
  (let ((mysql (mysql-configuration-mysql config))
        (socket-file (mysql-configuration-socket config))
        (config-file (mysql-configuration-file config)))
    (program-file
     "mysql-upgrade-wrapper"
     #~(begin
         (let ((mysql-upgrade #$(file-append mysql "/bin/mysql_upgrade"))
               (timeout 20))
           (begin
             (let loop ((i 0))
               (catch 'system-error
                 (lambda ()
                   (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                     (connect sock AF_UNIX #$socket-file)
                     (close-port sock)
                     ;; The socket is ready!
                     (execl mysql-upgrade mysql-upgrade
                            (string-append "--defaults-file=" #$config-file)
                            "--user=mysql")))
                 (lambda args
                   (if (< i timeout)
                       (begin
                         (sleep 1)
                         (loop (+ 1 i)))
                       ;; No luck, give up.
                       (throw 'timeout-error
                              "MySQL server did not appear in time!")))))))))))

(define (mysql-upgrade-shepherd-service config)
  (list (shepherd-service
         (provision '(mysql-upgrade))
         (requirement '(mysql))
         (one-shot? #t)
         (documentation "Upgrade MySQL database schemas.")
         (start #~(make-forkexec-constructor
                   (list #$(mysql-upgrade-wrapper config))
                   #:user "mysql" #:group "mysql"
                   #:log-file "/var/log/mysql_upgrade.log")))))

(define (mysql-shepherd-services config)
  (let ((mysql-services (mysql-shepherd-service config)))
    (if (mysql-configuration-auto-upgrade? config)
        (append mysql-services
                (mysql-upgrade-shepherd-service config))
        mysql-services)))

(define mysql-service-type
  (service-type
   (name 'mysql)
   (extensions
    (list (service-extension account-service-type
                             (const %mysql-accounts))
          (service-extension shepherd-root-service-type
                             mysql-shepherd-services)))
   (default-value (mysql-configuration))
   (description "Run the MySQL or MariaDB database server,
@command{mysqld}.")))

(define-deprecated (mysql-service #:key (config (mysql-configuration)))
  mysql-service-type
  (service mysql-service-type config))


;;;
;;; Redis
;;;

(define-record-type* <redis-configuration>
  redis-configuration make-redis-configuration
  redis-configuration?
  (redis             redis-configuration-redis ;file-like
                     (default redis))
  (bind              redis-configuration-bind
                     (default "127.0.0.1"))
  (port              redis-configuration-port
                     (default 6379))
  (working-directory redis-configuration-working-directory
                     (default "/var/lib/redis"))
  (config-file       redis-configuration-config-file
                     (default #f)))

(define (default-redis.conf bind port working-directory)
  (mixed-text-file "redis.conf"
                   "bind " bind "\n"
                   "port " (number->string port) "\n"
                   "dir " working-directory "\n"
                   "daemonize no\n"))

(define %redis-accounts
  (list (user-group (name "redis") (system? #t))
        (user-account
         (name "redis")
         (group "redis")
         (system? #t)
         (comment "Redis server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define redis-activation
  (match-lambda
    (($ <redis-configuration> redis bind port working-directory config-file)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "redis")))
           (mkdir-p #$working-directory)
           (chown #$working-directory (passwd:uid user) (passwd:gid user)))))))

(define redis-shepherd-service
  (match-lambda
    (($ <redis-configuration> redis bind port working-directory config-file)
     (let ((config-file
            (or config-file
                (default-redis.conf bind port working-directory))))
       (list (shepherd-service
              (provision '(redis))
              (documentation "Run the Redis daemon.")
              (requirement '(user-processes syslogd))
              (actions (list (shepherd-configuration-action config-file)))
              (start #~(make-forkexec-constructor
                        '(#$(file-append redis "/bin/redis-server")
                          #$config-file)
                        #:user "redis"
                        #:group "redis"))
              (stop #~(make-kill-destructor))))))))

(define redis-service-type
  (service-type (name 'redis)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          redis-shepherd-service)
                       (service-extension activation-service-type
                                          redis-activation)
                       (service-extension account-service-type
                                          (const %redis-accounts))))
                (default-value (redis-configuration))
                (description "Run Redis, a caching key/value store.")))
