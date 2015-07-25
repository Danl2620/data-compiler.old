#lang racket/base

;;(require racket/bytes)

(provide crc32/bytes
         crc32/string)

(define (crc32/bytes data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
       ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define (crc32/string s)
  (crc32/bytes (string->bytes/utf-8 s)))
