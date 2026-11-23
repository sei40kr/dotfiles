;; When the terminal regains focus, reload buffers whose file changed on disk.
;; A buffer with unsaved changes asks first; one whose file was deleted is
;; kept as is.

(require-builtin steel/time)
(require "helix/editor.scm")
(require "helix/misc.scm")

;; doc id -> the mtime already asked about, so declining isn't asked again
;; until the file changes once more
(define asked (hash))

(define (changed-mtime doc)
  (define path (editor-document->path doc))
  (and path
       (path-exists? path)
       (let ([mtime (fs-metadata-modified (file-metadata path))]
             [asked-mtime (hash-try-get asked (doc-id->usize doc))])
         (and (system-time>? mtime (editor-document-last-saved doc))
              (or (not asked-mtime) (system-time>? mtime asked-mtime))
              mtime))))

(define (ask-reload doc mtime)
  (set! asked (hash-insert asked (doc-id->usize doc) mtime))
  (push-component!
   (prompt (string-append (editor-document->path doc)
                          " changed on disk and has unsaved changes. Load it? (y/N) ")
           (lambda (answer)
             (when (member answer '("y" "Y"))
               (editor-document-reload doc))))))

(define (auto-reload)
  (for-each (lambda (doc)
              (define mtime (changed-mtime doc))
              (when mtime
                (if (editor-document-dirty? doc)
                    (ask-reload doc mtime)
                    (editor-document-reload doc))))
            (editor-all-documents)))

(register-hook 'terminal-focus-gained auto-reload)
