;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2022, 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix git-authenticate)
  #:use-module (git)
  #:autoload   (gcrypt hash) (sha256)
  #:use-module (guix base16)
  #:autoload   (guix base64) (base64-encode)
  #:use-module ((guix git)
                #:select (commit-difference
                          commit-descendant?
                          false-if-git-not-found))
  #:use-module (guix i18n)
  #:use-module ((guix diagnostics) #:select (formatted-message
                                             leave))
  #:use-module (guix openpgp)
  #:use-module ((ssh key) #:prefix ssh:)
  #:use-module ((guix utils)
                #:select (cache-directory with-atomic-file-output))
  #:use-module ((guix build utils)
                #:select (mkdir-p))
  #:use-module (guix progress)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 pretty-print) (pretty-print)
  #:use-module (ice-9 rdelim)
  #:export (read-authorizations
            commit-signing-key
            commit-authorized-keys
            authenticate-commit
            authenticate-commits
            load-keyrings-from-reference
            previously-authenticated-commits
            cache-authenticated-commit

            repository-cache-key
            authenticate-repository

            git-authentication-error?
            git-authentication-error-commit
            unsigned-commit-error?
            unauthorized-commit-error?
            unauthorized-commit-error-signing-key
            signature-verification-error?
            signature-verification-error-keyring
            signature-verification-error-signature
            missing-key-error?
            missing-key-error-signature))

;;; Commentary:
;;;
;;; This module provides tools to authenticate a range of Git commits.  A
;;; commit is considered "authentic" if and only if it is signed by an
;;; authorized party.  Parties authorized to sign a commit are listed in the
;;; '.guix-authorizations' file of the parent commit.
;;;
;;; Code:

;; XXX: to get an SSH fingerprint: ssh-keygen -l -f ~/.ssh/id_rsa.pub
(define-record-type <fingerprint>
  (make-fingerprint key-type data)
  fingerprint?
  (key-type fingerprint-key-type) ;'openpgp | 'sshsig
  (data fingerprint-data))  ;bytevector (OpenPGP) or SSH string fingerprint

(define (fingerprint=? a b)
  (and (eq? (fingerprint-key-type a) (fingerprint-key-type b))
       (equal? (fingerprint-data a) (fingerprint-data b))))

(define-condition-type &git-authentication-error &error
  git-authentication-error?
  (commit  git-authentication-error-commit))

(define-condition-type &unsigned-commit-error &git-authentication-error
  unsigned-commit-error?)

(define-condition-type &unauthorized-commit-error &git-authentication-error
  unauthorized-commit-error?
  (signing-key unauthorized-commit-error-signing-key))

(define-condition-type &signature-verification-error &git-authentication-error
  signature-verification-error?
  (signature signature-verification-error-signature)
  (keyring   signature-verification-error-keyring))

(define-condition-type &missing-key-error &git-authentication-error
  missing-key-error?
  (signature missing-key-error-signature))

(define* (openpgp-commit-signing-key commit-id keyring signature signed-data
                                     #:key (disallowed-hash-algorithms '(sha1)))
  "Return the OpenPGP key that signed COMMIT-ID (an OID).  Raise an exception
if the commit is unsigned, has an invalid SIGNATURE, has a signature using one
of the hash algorithms in DISALLOWED-HASH-ALGORITHMS, or if its signing key is
not in KEYRING."
  (let ((signature (string->openpgp-packet signature)))
    (when (memq (openpgp-signature-hash-algorithm signature)
                `(,@disallowed-hash-algorithms md5))
      (raise (make-compound-condition
              (condition (&unsigned-commit-error (commit commit-id)))
              (formatted-message (G_ "commit ~a has a ~a signature, \
which is not permitted")
                                 (oid->string commit-id)
                                 (openpgp-signature-hash-algorithm
                                  signature)))))

    (with-fluids ((%default-port-encoding "UTF-8"))
      (let (((values status data)
             (verify-openpgp-signature signature keyring
                                       (open-input-string signed-data))))
        (match status
          ('bad-signature
           ;; There's a signature but it's invalid.
           (raise (make-compound-condition
                   (condition
                    (&signature-verification-error
                     (commit commit-id)
                     (signature signature)
                     (keyring keyring)))
                   (formatted-message (G_ "signature verification failed \
for commit ~a")
                                      (oid->string commit-id)))))
          ('missing-key
           (raise (make-compound-condition
                   (condition (&missing-key-error (commit commit-id)
                                                  (signature signature)))
                   (formatted-message (G_ "could not authenticate \
commit ~a: key ~a is missing")
                                      (oid->string commit-id)
                                      (openpgp-format-fingerprint data)))))
          ('good-signature data))))))

(define (ssh-public-key=? a b)
  (string=? (ssh:public-key->string a)
            (ssh:public-key->string b)))

(define (sshsig-commit-signing-key commit-id keyring signature signed-data)
  "Return the SSHSIG key that signed COMMIT-ID (an OID).  Raise an exception
if the commit is unsigned, has an invalid signature, or if its signing key is
not in KEYRING."
  (catch 'guile-ssh-error
         (lambda ()
           ;; XXX: We don't perform similar hash algorithm verification
           ;; here for now, because we don't have a public API for that
           ;; in libssh for now.  However, we are mostly fine because
           ;; md5 and sha1 are already disabled by OpenSSH and libssh.
           ;; See libssh commit 7a2a743a39fab3c044343b036560008f3e00e955
           ;; for the expected error message.
           (match (ssh:verify signed-data signature #:namespace "git")
             (#f
              ;; There's a signature but it's invalid.
              (raise (make-compound-condition
                      (condition
                       (&signature-verification-error
                        (commit commit-id)
                        (signature signature)
                        (keyring keyring)))
                      (formatted-message (G_ "signature verification failed \
for commit ~a")
                                         (oid->string commit-id)))))
             ((? (lambda (key)
                   (any (cut ssh-public-key=? key <>) keyring))
                 public-key)
              public-key)
             (_
              ;; There's a signature but it's invalid.
              (raise (make-compound-condition
                      (condition
                       (&signature-verification-error
                        (commit commit-id)
                        (signature signature)
                        (keyring keyring)))
                      (formatted-message (G_ "signature verification failed \
                  for commit ~a")
                                         (oid->string commit-id)))))))
         (lambda (key . args)
           (format #t "~a: ~a~%" key args)
           #f)))

(define* (commit-signing-key repo commit-id keyrings
                             #:key (disallowed-hash-algorithms '(sha1)))
  "Return the key that signed COMMIT-ID (an OID).  Raise an exception if the
commit is unsigned, has an invalid signature, has a signature using one of the
hash algorithms in DISALLOWED-HASH-ALGORITHMS, or if its signing key is not in
KEYRING."
  (let (((values signature signed-data)
         (catch 'git-error
                (lambda ()
                  (commit-extract-signature repo commit-id))
                (lambda _
                  (values #f #f)))))
    (unless signature
      (raise (make-compound-condition
              (condition (&unsigned-commit-error (commit commit-id)))
              (formatted-message (G_ "commit ~a lacks a signature")
                                 (oid->string commit-id)))))

    (match signature
      ((? (cut string-prefix? "-----BEGIN PGP SIGNATURE-----" <>))
       (openpgp-commit-signing-key commit-id (assoc-ref keyrings 'openpgp)
                                   signature signed-data
                                   #:disallowed-hash-algorithms
                                   disallowed-hash-algorithms))
      ((? (cut string-prefix? "-----BEGIN SSH SIGNATURE-----" <>))
       (sshsig-commit-signing-key commit-id (assoc-ref keyrings 'sshsig)
                                  signature signed-data)))))

(define (openpgp-process-fingerprint fingerprint)
  "Helper to process OpenPGP fingerprints."
  (base16-string->bytevector
   (string-downcase (string-filter char-set:graphic fingerprint))))

(define (read-authorizations port)
  "Read authorizations in the '.guix-authorizations' format from PORT, and
return a list of authorized fingerprints."
  (match (read port)
    (('authorizations ('version 0)
                      (((? string? fingerprints) _ ...) ...)
                      _ ...)
     (map openpgp-process-fingerprint fingerprints))
    (('authorizations ('version 1)
                      (((? string? fingerprints) . rests) ...)
                      _ ...)
     (map
       (lambda (fingerprint rest)
         (match (or (assoc-ref rest 'format) (list 'openpgp))
           (('openpgp)
            (make-fingerprint 'openpgp
                               (openpgp-process-fingerprint fingerprint)))
           (('sshsig)
            (make-fingerprint 'sshsig fingerprint))
           (_
            (leave (G_ "read_authorizations: Only 'openpgp and 'sshsig key types are supported.")))))
       fingerprints rests))))

(define* (commit-authorized-keys repository commit
                                 #:optional (default-authorizations '()))
  "Return the list of OpenPGP fingerprints authorized to sign COMMIT, based on
authorizations listed in its parent commits.  If one of the parent commits
does not specify anything, fall back to DEFAULT-AUTHORIZATIONS."
  (define (parents-have-authorizations-file? commit)
    ;; Return true if at least one of the parents of COMMIT has the
    ;; '.guix-authorizations' file.
    (find (lambda (commit)
            (false-if-git-not-found
             (tree-entry-bypath (commit-tree commit)
                                ".guix-authorizations")))
          (commit-parents commit)))

  (define (assert-parents-lack-authorizations commit)
    ;; If COMMIT removes the '.guix-authorizations' file found in one of its
    ;; parents, raise an error.
    (when (parents-have-authorizations-file? commit)
      (raise (make-compound-condition
              (condition
               (&unauthorized-commit-error (commit (commit-id commit))
                                           (signing-key #f)))
              (formatted-message (G_ "commit ~a attempts \
to remove '.guix-authorizations' file")
                                 (oid->string (commit-id commit)))))))

  (define (commit-authorizations commit)
    (catch 'git-error
      (lambda ()
        (let* ((tree  (commit-tree commit))
               (entry (tree-entry-bypath tree ".guix-authorizations"))
               (blob  (blob-lookup repository (tree-entry-id entry))))
          (read-authorizations
           (open-bytevector-input-port (blob-content blob)))))
      (lambda (key error)
        (if (= (git-error-code error) GIT_ENOTFOUND)
            (begin
              ;; Prevent removal of '.guix-authorizations' since it would make
              ;; it trivial to force a fallback to DEFAULT-AUTHORIZATIONS.
              (assert-parents-lack-authorizations commit)
              default-authorizations)
            (throw key error)))))

  (match (commit-parents commit)
    (() default-authorizations)
    (parents
     (apply lset-intersection fingerprint=?
            (map commit-authorizations parents)))))

(define* (authenticate-commit repository commit keyrings
                              #:key (default-authorizations '()))
  "Authenticate COMMIT from REPOSITORY and return the signing key fingerprint.
Raise an error when authentication fails.  If one of the parent commits does
not specify anything, fall back to DEFAULT-AUTHORIZATIONS."
  (define id
    (commit-id commit))

  (define recent-commit?
    (false-if-git-not-found
     (tree-entry-bypath (commit-tree commit) ".guix-authorizations")))

  (define signing-key
    (commit-signing-key repository id keyrings
                        ;; Reject SHA1 signatures unconditionally as suggested
                        ;; by the authors of "SHA-1 is a Shambles" (2019).
                        ;; Accept it for "historical" commits (there are such
                        ;; signatures from April 2020 in the repository).
                        #:disallowed-hash-algorithms
                        (if recent-commit? '(sha1) '())))

  (let* ((authorized-keys (commit-authorized-keys
                           repository commit default-authorizations))
         ((values authorized? do-format)
          (match signing-key
            ((? openpgp-public-key?)
             (let ((fingerprint (openpgp-public-key-fingerprint signing-key)))
               (values (member fingerprint authorized-keys)
                       (compose openpgp-format-fingerprint
                                openpgp-public-key-fingerprint))))
            ((? ssh:key?)
             (let ((fingerprint (ssh:get-public-key-fingerprint signing-key)))
               (values
                (any (lambda (cand)
                       (and (not (bytevector? cand))  ; XXX: version 0
                            (eq? (fingerprint-key-type cand) 'sshsig)
                            (string= (fingerprint-data cand) fingerprint)))
                     authorized-keys)
                ssh:public-key->string))))))
    (unless authorized?
      (raise (make-compound-condition
              (condition
               (&unauthorized-commit-error (commit id)
                                           (signing-key signing-key)))
              (formatted-message (G_ "commit ~a not signed by an authorized \
key: ~a")
                                 (oid->string id)
                                 (do-format signing-key))))))

  signing-key)

(define (load-keyring-from-port port entry keyring)
  "Augment KEYRING with the keyring available in ENTRY (a tree entry), which
may or may not be ASCII-armored."
  (get-openpgp-keyring (if (port-ascii-armored? port)
                           (match (read-radix-64 port)
                             ((? bytevector? radix)
                              (open-bytevector-input-port radix))
                             (_
                              (raise
                               (formatted-message (G_ "malformed \
ASCII-armored key in ~a (blob ~a)")
                                                  (tree-entry-name entry)
                                                  (oid->string
                                                   (tree-entry-id entry))))))
                           port)
                       keyring))

(define (load-keyrings-from-reference repository reference)
  "Load the '.key' files from the tree at REFERENCE in REPOSITORY and return
an alist of OpenPGP and SSHSIG keyrings."
  (let* ((reference (branch-lookup repository reference BRANCH-ALL))
         (target    (reference-target reference))
         (commit    (commit-lookup repository target))
         (tree      (commit-tree commit)))
    (fold
     (lambda (name keyrings)
       (if (string-suffix? ".key" name)
           (let* ((entry (tree-entry-bypath tree name))
                  (oid (tree-entry-id entry))
                  (blob (blob-lookup repository oid))
                  (port (open-bytevector-input-port
                         (blob-content blob)))
                  (line (read-line port))
                  ;; XXX: Re-generate port.
                  (port (open-bytevector-input-port
                         (blob-content blob))))
             (match line
               ("-----BEGIN PGP PUBLIC KEY BLOCK-----"
                (list
                 (cons
                  'openpgp
                  (load-keyring-from-port port
                                          entry
                                          (assoc-ref keyrings 'openpgp)))
                 (assoc 'sshsig keyrings)))
               ((? cut string-prefix? "ssh-" <>)
                (match (string-split (get-string-all port) #\ )
                  ((type data . rest)
                   (let* ((type (string-drop type (string-length "ssh-")))
                          (type (string->symbol type)))
                     (list (assoc 'openpgp keyrings)
                           (cons* 'sshsig
                                  (ssh:string->public-key data type)
                                  (assoc-ref keyrings 'sshsig)))))))
               (_
                (raise (formatted-message (G_ "could not read invalid \
key ~a")
                                          name)))))
           keyrings))
     `((openpgp . ,%empty-keyring)
       (sshsig  . '()))
     (tree-list tree))))

(define* (authenticate-commits repository commits
                               #:key
                               (default-authorizations '())
                               (keyring-reference "keyring")
                               (keyrings (load-keyrings-from-reference
                                          repository keyring-reference))
                               (report-progress (const #t)))
  "Authenticate COMMITS, a list of commit objects, calling REPORT-PROGRESS for
each of them.  Return an alist showing the number of occurrences of each key.
If KEYRING is omitted, the OpenPGP keyring is loaded from KEYRING-REFERENCE in
REPOSITORY."
  (fold (lambda (commit stats)
          (report-progress)
          (let ((signer (authenticate-commit repository commit keyrings
                                             #:default-authorizations
                                             default-authorizations)))
            (match (assq signer stats)
              (#f          (cons `(,signer . 1) stats))
              ((_ . count) (cons `(,signer . ,(+ count 1))
                                 (alist-delete signer stats))))))
        '()
        commits))


;;;
;;; Caching.
;;;

(define (authenticated-commit-cache-file key)
  "Return the name of the file that contains the cache of
previously-authenticated commits for KEY."
  (string-append (cache-directory) "/authentication/" key))

(define (previously-authenticated-commits key)
  "Return the previously-authenticated commits under KEY as a list of commit
IDs (hex strings)."
  (catch 'system-error
    (lambda ()
      (call-with-input-file (authenticated-commit-cache-file key)
        (lambda (port)
          ;; If PORT has the wrong permissions, it might have been tampered
          ;; with by another user so ignore its contents.
          (if (= #o600 (stat:perms (stat port)))
              (read port)
              (begin
                (chmod port #o600)
                '())))))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          '()
          (apply throw args)))))

(define (cache-authenticated-commit key commit-id)
  "Record in ~/.cache, under KEY, COMMIT-ID and its closure as
authenticated (only COMMIT-ID is written to cache, though)."
  (define %max-cache-length
    ;; Maximum number of commits in cache.
    200)

  (let ((lst  (delete-duplicates
               (cons commit-id (previously-authenticated-commits key))))
        (file (authenticated-commit-cache-file key)))
    (mkdir-p (dirname file))
    (with-atomic-file-output file
      (lambda (port)
        (let ((lst (if (> (length lst) %max-cache-length)
                       (take lst %max-cache-length) ;truncate
                       lst)))
          (chmod port #o600)
          (display ";; List of previously-authenticated commits.\n\n"
                   port)
          (pretty-print lst port)))
      #:sync? #f)))


;;;
;;; High-level interface.
;;;

(define (repository-cache-key repository)
  "Return a unique key to store the authenticate commit cache for REPOSITORY."
  (string-append "checkouts/"
                 (base64-encode
                  (sha256 (string->utf8 (repository-directory repository))))))

(define (verify-introductory-commit repository keyrings commit expected-signer)
  "Look up COMMIT in REPOSITORY, and raise an exception if it is not signed by
EXPECTED-SIGNER."
  (define actual-signer
    (openpgp-public-key-fingerprint
     (commit-signing-key repository (commit-id commit) keyrings)))

  (unless (bytevector=? expected-signer actual-signer)
    (raise (formatted-message (G_ "initial commit ~a is signed by '~a' \
instead of '~a'")
                              (oid->string (commit-id commit))
                              (openpgp-format-fingerprint actual-signer)
                              (openpgp-format-fingerprint expected-signer)))))

(define* (authenticate-repository repository start signer
                                  #:key
                                  (keyring-reference "keyring")
                                  (cache-key (repository-cache-key repository))
                                  (end (reference-target
                                        (repository-head repository)))
                                  (authentic-commits '())
                                  (historical-authorizations '())
                                  (make-reporter
                                   (const progress-reporter/silent)))
  "Authenticate REPOSITORY up to commit END, an OID.  Authentication starts
with commit START, an OID, which must be signed by SIGNER; an exception is
raised if that is not the case.  Commits listed in AUTHENTIC-COMMITS and their
closure are considered authentic.  Return an alist mapping OpenPGP public keys
to the number of commits signed by that key that have been traversed.

The OpenPGP keyring is loaded from KEYRING-REFERENCE in REPOSITORY, where
KEYRING-REFERENCE is the name of a branch.  The list of authenticated commits
is cached in the authentication cache under CACHE-KEY.

HISTORICAL-AUTHORIZATIONS must be a list of <fingerprints> denoting the
authorized keys for commits whose parent lack the '.guix-authorizations' file."
  (define start-commit
    (commit-lookup repository start))
  (define end-commit
    (commit-lookup repository end))

  (define keyrings
    (load-keyrings-from-reference repository keyring-reference))

  (define authenticated-commits
    ;; Previously-authenticated commits that don't need to be checked again.
    (filter-map (lambda (id)
                  (false-if-git-not-found
                   (commit-lookup repository (string->oid id))))
                (append (previously-authenticated-commits cache-key)
                        authentic-commits)))

  (define commits
    ;; Commits to authenticate, excluding the closure of
    ;; AUTHENTICATED-COMMITS.
    (commit-difference end-commit start-commit
                       authenticated-commits))

  ;; When COMMITS is empty, it's because END-COMMIT is in the closure of
  ;; START-COMMIT and/or AUTHENTICATED-COMMITS, in which case it's known to
  ;; be authentic already.
  (if (null? commits)
      '()
      (let ((reporter (make-reporter start-commit end-commit commits)))
        ;; If it's our first time, verify START-COMMIT's signature.
        (when (null? authenticated-commits)
          (verify-introductory-commit repository keyrings
                                      start-commit signer))

        ;; Make sure END-COMMIT is a descendant of START-COMMIT or of one of
        ;; AUTHENTICATED-COMMITS, which are known to be descendants of
        ;; START-COMMIT.
        (unless (commit-descendant? end-commit
                                    (cons start-commit
                                          authenticated-commits))
          (raise (formatted-message
                  (G_ "commit ~a is not a descendant of introductory commit ~a")
                  (oid->string (commit-id end-commit))
                  (oid->string (commit-id start-commit)))))

        (let ((stats (call-with-progress-reporter reporter
                       (lambda (report)
                         (authenticate-commits repository commits
                                               #:keyrings keyrings
                                               #:default-authorizations
                                               historical-authorizations
                                               #:report-progress report)))))
          (cache-authenticated-commit cache-key
                                      (oid->string (commit-id end-commit)))

          stats))))
