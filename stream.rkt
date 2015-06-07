#lang racket/base

[require
 (for-syntax racket/base
             syntax/parse
             racket/syntax
             racket/list)
 racket/bytes
 racket/file
 racket/list
 ]

(define *big-endian* #f)

(define (bytes-crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
       ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define (crc32 s)
  (bytes-crc32 (string->bytes/utf-8 s)))


(define (int32->bytes v) (integer->integer-bytes v 4 #t *big-endian*))
(define (bytes->int32 bs)
  (integer-bytes->integer bs
                          #t
                          *big-endian*
                          0
                          4))


(define (int64->bytes v) (integer->integer-bytes v 8 #t *big-endian*))
(define (bytes->int64 bs)
  (integer-bytes->integer bs
                          #t
                          *big-endian*
                          0
                          8))

(define (word32->bytes v) (integer->integer-bytes v 4 #f))
(define (bytes->word32 bs)
  (integer-bytes->integer bs
                          #f
                          *big-endian*
                          0
                          4))

(define (word64->bytes v) (integer->integer-bytes v 8 #f))
(define (bytes->word64 bs)
  (integer-bytes->integer bs
                          #f
                          *big-endian*
                          0
                          8))

;;(struct type (size ->bytes))

(struct type (size read write))

(define (make-type #:size size
                   #:read read
                   #:write write)
  (type size read write))

;; read : (-> input-port? any)
;;write	: (or/c
 ;; (->* (output-port? any/c) #:rest list? void?)
 ;; (-> output-port? any/c void?))

(define (read-number length bytes->num)
  (lambda (in)
    (bytes->num (read-bytes length in))
    ))

(define (write-number num->bytes)
  (lambda (val out)
    (display (num->bytes val) out)))


(define int32 (make-type #:size 4 #:read (read-number 4 bytes->int32) #:write (write-number int32->bytes)))
(define int64 (make-type #:size 8 #:read (read-number 8 bytes->int64) #:write (write-number int64->bytes)))
(define word32 (make-type #:size 4 #:read (read-number 4 bytes->word32) #:write (write-number word32->bytes)))
(define word64 (make-type #:size 8 #:read (read-number 8 bytes->word64) #:write (write-number word64->bytes)))

(struct instance (type value))

;;
;; payload needs to be an instance -- an array of values, or a defined struct
;;

(define payload
  (list (instance int32 24)
        (instance word64 #xffffffffffffffff)))

(define (generate-header-bytes bs bs-port)
  [bytes-append
   #"DC00"
   (int32->bytes 1)
   (int32->bytes (+ 16 (foldl + 0 (map (compose type-size instance-type) bs))))
   (word32->bytes (bytes-crc32 (get-output-bytes bs-port)))
   ])

(define (main)
  ;;[display (equal? 12 (int32-decode (encode-closure int32-encode 12) 0))]

  (let ((payload-port (open-output-bytes)))
    (for-each (lambda (inst)
                ((type-write (instance-type inst)) (instance-value inst) payload-port))
              payload)

    (call-with-atomic-output-file
     (build-path "test.bin")
     (lambda (port tmp-path)
       (display (generate-header-bytes payload payload-port) port)
       (display (get-output-bytes payload-port) port)
       ;;(printf "bb : ~v~n" bb)
       ;;(display bb port)
       )))

  ;;(write (string->bytes/utf-8 (format "test string~n")))

  ;; [write-to-file (string->bytes/utf-8 (format "test string~n"))
  ;;                (build-path "test.bin")
  ;;                #:mode 'binary]
  )

(main)
