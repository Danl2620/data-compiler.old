#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     syntax/parse)
         "../fracas.rkt")

;; The only purpose of this is to (a) make sure that every Fracas module
;; has the right submodules and (b) make sure that the Fracas code has the
;; right imports. Nothing exciting going on.

(define-syntax (:module-begin stx)
  (syntax-parse stx
    [(mb body0 . body)
     (quasisyntax/loc stx
       (#%module-begin
        ;; Provide all definitions, but with an `#%auto:` prefix to hide them:
        (provide #,(datum->syntax #'body0 '(prefix-out #%auto: (all-defined-out))))
        ;; The `auto-exports` submodule exports all of a module's definitions
        ;; and everything that it imports
        (module+ auto-exports
          (require racket/require)
          (require (filtered-in (lambda (name)
                                  (and (regexp-match? #rx"^#%auto:" name)
                                       (regexp-replace #rx"^#%auto:" name "")))
                                (submod "..")))
          (provide (all-from-out (submod ".."))))
        body0 . body))]))

(provide
 (rename-out [:module-begin #%module-begin])
 (except-out (all-from-out "../fracas.rkt")
             #%module-begin))
