#lang racket/base

[require
 (for-syntax racket/base
             syntax/parse
             racket/syntax
             racket/list)
 racket/bytes
 racket/file
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

;; (begin-for-syntax
;;   (define-syntax-class spec
;;     #:attributes (encode decode size)

;;     [pattern
;;      ((~literal struct)
;;       (name0:id type0:spec)
;;       ...
;;       (~optional (~seq #:align align-e:expr)))

;;      #:with ((e.start e.end) ...)
;;      (let ()

;;        )

;;      #:size (apply + (attribute type0.size))
;;      #:attr encode
;;      (with-syntax
;;          ([((i e.bs e.tree) ...)
;;            (for/list ([i (in-naturals)]
;;                       [e (in-list (syntax->list #'(type0 ...)))])
;;              (list i (generate-temporary) (generate-temporary)))])
;;        #'(lambda (v off)
;;            (let-values ([(e.bs e.tree)
;;                          (type0.encode (vector-ref v i))])
;;              )))

;;      ]
;;     ))


;; (define hash-table
;;   (struct
;;     #:private-fields
;;     (count : int32)
;;     (values : (value array))

;;     #:constructor
;;     (lambda ((value list) -> hash-table)
;;       (new hash-table
;;            (count (length ))
;;       )

;;     #:method lookup
;;     (lambda (symbol -> value)
;;       (let ((index (symbol->hash# id)))
;;         (aref (-> self values) index))
;;       ))))




(define (main)
  ;;[display (equal? 12 (int32-decode (encode-closure int32-encode 12) 0))]

  (call-with-atomic-output-file
   (build-path "test.bin")
   (lambda (port tmp-path)

     (let ((bs [bytes-append
                #"DC00"
                (int32->bytes 1)
                (int32->bytes 20)
                ]))

       ;;(string->bytes/utf-8 (format "test string~n"))
       (display bs port)

       (display
        (let ((lst (list (instance int32 24)
                         (instance word64 #xffffffffffffffff))))
          (bytes-append*
           (map (lambda (inst)
                  ((type-->bytes (instance-type inst)) (instance-value inst)))
                lst)
           ))
        port
        )
       )
     ))

  ;;(write (string->bytes/utf-8 (format "test string~n")))

  ;; [write-to-file (string->bytes/utf-8 (format "test string~n"))
  ;;                (build-path "test.bin")
  ;;                #:mode 'binary]
  )

(main)
