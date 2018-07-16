#lang racket/base

(require
  racket/bytes
  racket/list
  "integer.rkt"
  "types.rkt"
  "crc32.rkt"
  "encode.rkt"
  )

(provide make-module-bytes
         fracas-module)


(struct fracas-module
  (names
   values))

(define (make-empty-module)
  (fracas-module '#() '#()))

;; (define (register-export! id)
;;   (add-export! *current-module* id))

(define (form-offsets lens)
  (reverse (foldl (lambda (el lst) (cons (+ el (car lst)) lst)) '(0) lens)))

;; header:
;;  magic u4
;;  version i4
;;  size i4
;;  count i4
;;  crc32 u4

(define (generate-header->bytes mod)
  (let ((count (vector-length (fracas-module-values mod))))
    [bytes-append
     #"Frac"
     (int32->bytes 1)
     (int32->bytes (+ (* 8 count) (for/sum ((inst (vector->list (fracas-module-values mod)))) (size-of inst))))
     (int32->bytes count)
     (word32->bytes #xcafebabe)
     ]))

(define (make-module-bytes mod)
  (let* ((header-bytes (generate-header->bytes mod))
         (values-list (vector->list (fracas-module-values mod)))
         (bss (map encode values-list))
         (lens (map bytes-length bss))
         
         (payload-bytes (bytes-append
                         (bytes-append
                          ;; write name/offset pairs
                          (bytes-append*
                           (map
                            (lambda (name len)
                              (bytes-append
                               (word32->bytes (crc32/string (symbol->string name)))
                               (word32->bytes len)
                               ))
                            (vector->list (fracas-module-names mod))
                            (drop-right (form-offsets lens) 1)
                            ))
                          
                          ;; write items
                          (bytes-append* bss))
                         )))
    (bytes-append header-bytes payload-bytes)
    ))

(module+ test
  (require rackunit)
  (let ((mt-mod (make-empty-module)))
    (check-eq? (vector-length (fracas-module-values mt-mod)) 0 "empty module")
    )
  
  )