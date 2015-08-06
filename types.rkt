#lang racket/base

[require
 "params.rkt"
 "integer.rkt"
 ]

[provide
 (struct-out type)
 (struct-out instance)
 (struct-out addr*thunk)
 size-of
 int32
 int64
 word32
 word64
 real32
 string
 pointer
 ]

(struct type (read write size-of))
(struct instance (type value))

(define (make-type #:read read
                   #:write write
                   #:size-of size-of)
  (type read write size-of))

;; read  : (-> bytes start any)
;; write : (-> any/c offset void?)

(struct addr*thunk (addr thunk))

(define (read-number bytes->num)
  (lambda (bs start)
    (bytes->num bs #:offset start)))

(define (write-number num->bytes)
  (lambda (val off)
    (values (num->bytes val) '())))

(define-syntax-rule (make-numeric-type size read write)
  (make-type #:read (read-number read) #:write (write-number write) #:size-of (lambda (inst) size)))

(define int32  (make-numeric-type 4 bytes->int32 int32->bytes))
(define int64  (make-numeric-type 8 bytes->int64 int64->bytes))
(define word32 (make-numeric-type 4 bytes->word32 word32->bytes))
(define word64 (make-numeric-type 8 bytes->word64 word64->bytes))
(define real32 (make-numeric-type 4 bytes->real32 real32->bytes))
(define string (make-type #:read (lambda (bs start)
                                   (let* ((ptr (bytes->word64 bs #:offset start))
                                          (len (bytes->int16 bs #:offset ptr)))
                                     (bytes->string/utf-8 bs #f (+ ptr 2) (+ ptr 2 len))))
                          #:write (lambda (val off)
                                    (values
                                     (word64->bytes #xffffffffffffffff)
                                     (list (addr*thunk
                                            off
                                            (lambda (thunk-off)
                                              (values (bytes-append
                                                       (int16->bytes (string-length val))
                                                       (string->bytes/utf-8 val))
                                                      '()))))))
                          #:size-of (lambda (inst)
                                      (+ 2 (string-length (instance-value inst)))
                                      )))
(define (pointer type)
  (make-type #:read (lambda (bs start)
                      (let ((offset (bytes->word64 bs #:offset start)))
                        ((type-read) bs offset)))
             #:write (lambda (val off)
                       (values (word64->bytes #xffffffffffffffff)
                               (list (addr*thunk
                                      off
                                      (λ (thunk-off)
                                        ((type-write type) val thunk-off)
                                        )))))
             #:size-of (lambda (inst) 8)))

(module+ test
  (require rackunit)

  (test-case "int32"
             (check-equal? ((type-read int32) (let-values [((bs unused) ((type-write int32) 12 0))]
                                                          bs) 0) 12)
             )
  (test-case "int32"
             (check-equal? ((type-read int32) (let-values [((bs unused) ((type-write int32) -82447 0))]
                                                bs) 0) -82447)
             )
  )

(define (size-of inst)
  ((type-size-of (instance-type inst)) inst))
