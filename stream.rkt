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

(define (int32->bytes v) (integer->integer-bytes v 4 #t))
(define (int64->bytes v) (integer->integer-bytes v 8 #t))
(define (word32->bytes v) (integer->integer-bytes v 4 #f))
(define (word64->bytes v) (integer->integer-bytes v 8 #f))

(struct type (size ->bytes))

(define int32 (type 4 int32->bytes))
(define int64 (type 8 int64->bytes))
(define word32 (type 4 word32->bytes))
(define word64 (type 8 word64->bytes))

(struct instance (type value))

;;
;; payload needs to be an instance -- an array of values, or a defined struct
;;

(define payload
  (list (instance int32 24)
        (instance word64 #xffffffffffffffff)))

(define (generate-header-bytes bs)
  [bytes-append
   #"DC00"
   (int32->bytes 1)
   (int32->bytes (+ 12 (foldl + 0 (map (compose type-size instance-type) bs))))
   ])

(define (main)
  ;;[display (equal? 12 (int32-decode (encode-closure int32-encode 12) 0))]

  (call-with-atomic-output-file
   (build-path "test.bin")
   (lambda (port tmp-path)
     (display (generate-header-bytes payload) port)
     (let ((bb (bytes-append*
                (map (lambda (inst)
                       ((type-->bytes (instance-type inst)) (instance-value inst)))
                     payload)
                )))
       ;;(printf "bb : ~v~n" bb)
       (display bb port)
       )))

  ;;(write (string->bytes/utf-8 (format "test string~n")))

  ;; [write-to-file (string->bytes/utf-8 (format "test string~n"))
  ;;                (build-path "test.bin")
  ;;                #:mode 'binary]
  )

(main)
