#lang racket/base

(require
 racket/match
 "params.rkt"
 "types.rkt"
 )

(provide
 encode
 decode)

(struct branch (left right))

(define (stitch vbs addr*et-tree)
  (match addr*et-tree
    [(list)
     vbs]
    [(list-rest (addr*thunk addr et) more)
     ;;(printf "vbs: ~v~n" vbs)
     (define start (bytes-length vbs))
     (define-values (et-bs et-mores) (et start))
     (define nbs (bytes-append vbs et-bs))
     ;; Replace addr in nbs with addr (patch)
     ;;(printf "nbs: ~v~n" nbs)
     (integer->integer-bytes start 8 #f (*big-endian*) nbs addr)
;;     (integer->integer-bytes #x87654321 4 #f #t nbs addr)
     (stitch nbs (branch more et-mores))]
    [(branch (branch left-left-b left-right-b) right-b)
     (stitch vbs (branch left-left-b
                         (branch left-right-b
                                 right-b)))]
    [(branch (list) right-tree)
     (stitch vbs right-tree)]
    [(branch (list-rest first-on-left rest-of-left) right-tree)
     (stitch vbs
             (cons first-on-left
                   (branch rest-of-left
                           right-tree)))]))

(define (encode inst)
  (let-values [((bs tree) ((type-write (instance-type inst)) (instance-value inst) 0))]
                                           (stitch bs tree)))

(define (decode bs type)
  ((type-read type) bs 0))


(module+ test
  (require rackunit)

  (define (check-coding type val)
    (check-= val (decode (encode (instance type val)) type) 0))

  (test-case "int32"
             (check-coding int32 24)
             (check-coding int32 2147483647)
             (check-coding int32 0)
             (check-coding int32 -1241)
             (check-coding int32 -2147483648)
             )

  (test-case "word32"
             (check-coding word32 0)
             ;;(fail "more word32 tests needed!")
             )

  (test-case "int64"
             (check-coding int64 24)
             (check-coding int64 2147483647)
             (check-coding int64 4294967296)
             (check-coding int64 0)
             (check-coding int64 -1241)
             (check-coding int64 -2147483648)
             (check-coding int64 -4294967296)
             )

  (test-case "word64"
             (check-coding word64 24)
             (check-coding word64 2147483647)
             (check-coding word64 4294967296)
             (check-coding word64 0)
             (check-coding word64 9223372036854775808)
             )
  )
