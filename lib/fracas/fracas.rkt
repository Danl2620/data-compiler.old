#lang racket/base

(require (for-syntax racket/base))

(provide (all-from-out racket/base))

(define-syntax (define-export stx)
  (syntax-case stx ()
    ([_ id val]
     #'(let ()
         (define id (export-value id val))
         (register-export! id)))))
