(define-module (guix build privileged)
  #:use-module (gnu build activation)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:export (wrap-privileged))

;;; Move .xxx-real to xxx, if it exists.
(define (unwrap binary)
  (let* ((name (basename binary))
         (folder (dirname binary))
         (real (string-append folder "/." name "-real")))
    (when (file-exists? real)
      (format #t "Unwrapping ~a~%" binary)
      (rename-file real binary))))

;;;
;;; 1. Move {output}/{original} to {output}/{target-folder}/{target-name}.
;;; 2. Make a script at original-binary that executes /run/privileged/bin/{target-name}
;;;    if it exists, if not, output/{target-folder}/{target-name} is executed.
;;;
(define* (wrap-privileged output
                          original
                          target-name
                          #:key
                          (unwrap? #t)
                          (target-folder "privileged")
                          (privileged-directory %privileged-program-directory))
  "Make a shell wrapper for binary that should be ran as privileged.

The wrapper script will try executing binary in /run/privileged/bin, if it exists,
and if not, it will fall back to the original."
  (let ((original-file (string-append output "/" original))
        (target-file (string-append output "/" target-folder "/" target-name))
        (privileged-file (string-append privileged-directory "/" target-name)))
    (when unwrap?
      (unwrap original-file))
    (mkdir-p (dirname target-file))
    (rename-file original-file target-file)
    (call-with-output-file original-file
      (lambda (port)
        (format port "#!/usr/bin/env bash
if [[ -z \"$GUIX_SKIP_PRIVILEGED\" && -f \"~a\" ]]; then
  exec -a \"$0\" \"~a\" \"$@\"
fi

exec -a \"$0\" \"~a\" \"$@\"
" privileged-file privileged-file target-file)
        (chmod port #o555)))))
