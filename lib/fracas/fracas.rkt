#lang racket/base
(provide (all-from-out racket-base))

(define-syntax (define-export stx)
  (syntax-case stx ()
    ([_ id val]
     (define id (export-value id val))
     (register-export! id))))
