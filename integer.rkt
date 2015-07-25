#lang racket/base

(require
 "params.rkt"
 )

[provide
 int16->bytes
 bytes->int16
 int32->bytes
 bytes->int32
 int64->bytes
 bytes->int64
 word32->bytes
 bytes->word32
 word64->bytes
 bytes->word64
 real32->bytes
 bytes->real32
 ]

(define (int16->bytes v) (integer->integer-bytes v 2 #t (*big-endian*)))
(define (bytes->int16 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          (*big-endian*)
                          offset
                          (+ offset 2)))

(define (int32->bytes v) (integer->integer-bytes v 4 #t (*big-endian*)))
(define (bytes->int32 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          (*big-endian*)
                          offset
                          (+ offset 4)))


(define (int64->bytes v) (integer->integer-bytes v 8 #t (*big-endian*)))
(define (bytes->int64 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          (*big-endian*)
                          offset
                          (+ offset 8)))

(define (word32->bytes v) (integer->integer-bytes v 4 #f (*big-endian*)))
(define (bytes->word32 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #f
                          (*big-endian*)
                          offset
                          (+ offset 4)))

(define (word64->bytes v) (integer->integer-bytes v 8 #f (*big-endian*)))
(define (bytes->word64 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #f
                          (*big-endian*)
                          offset
                          (+ offset 8)))

(define (real32->bytes v) (real->floating-point-bytes v 4 (*big-endian*)))
(define (bytes->real32 bs #:offset (offset 0))
  (floating-point-bytes->real bs
                              (*big-endian*)
                              offset
                              (+ offset 4)))