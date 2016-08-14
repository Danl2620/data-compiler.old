#lang racket/base
(define (make-bin/path ip)
  (local-require racket/path
                 fracas/stream
                 )
  (define p (path->complete-path ip))
  (write-bin (path->string (file-name-from-path (path-replace-suffix p #""))) p))

(module+ main
  (require racket/cmdline)
  (command-line #:program "make-bin"
                #:args files
                (for-each make-bin/path files)))
