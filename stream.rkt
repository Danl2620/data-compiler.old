#lang racket/base

[require
 (for-syntax racket/base
             syntax/parse
             racket/syntax
             racket/list)
 racket/bytes
 racket/file
 racket/list
 racket/match
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

(define (int16->bytes v) (integer->integer-bytes v 2 #t *big-endian*))
(define (bytes->int16 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          *big-endian*
                          offset
                          (+ offset 2)))

(define (int32->bytes v) (integer->integer-bytes v 4 #t *big-endian*))
(define (bytes->int32 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          *big-endian*
                          offset
                          (+ offset 4)))


(define (int64->bytes v) (integer->integer-bytes v 8 #t *big-endian*))
(define (bytes->int64 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #t
                          *big-endian*
                          offset
                          (+ offset 8)))

(define (word32->bytes v) (integer->integer-bytes v 4 #f))
(define (bytes->word32 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #f
                          *big-endian*
                          offset
                          (+ offset 4)))

(define (word64->bytes v) (integer->integer-bytes v 8 #f))
(define (bytes->word64 bs #:offset (offset 0))
  (integer-bytes->integer bs
                          #f
                          *big-endian*
                          offset
                          (+ offset 8)))

(define (real32->bytes v) (real->floating-point-bytes v 4 *big-endian*))
(define (bytes->real32 bs #:offset (offset 0))
  (floating-point-bytes->real bs
                              *big-endian*
                              offset
                              (+ offset 4)))

;;(struct type (size ->bytes))

(struct type (read write size-of))
(struct instance (type value))

(define (make-type #:read read
                   #:write write
                   #:size-of size-of)
  (type read write size-of))

;; read  : (-> bytes start any)
;; write : (-> any/c offset void?)

(struct addr*thunk (addr thunk))
(struct branch (left right))

(define (stitch vbs addr*et-tree)
  (match addr*et-tree
    [(list)
     vbs]
    [(list-rest (addr*thunk addr et) more)
     (define start (bytes-length vbs))
     (define-values (et-bs et-mores) (et start))
     (define nbs (bytes-append vbs et-bs))
     ;; Replace addr in nbs with addr (patch)
     (integer->integer-bytes start 8 #f *big-endian* nbs addr)
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

(define (read-number bytes->num)
  (lambda (bs start)
    (bytes->num bs #:offset start)))

(define (write-number num->bytes)
  (lambda (val off)
    (values (num->bytes val) empty)))

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
                                                      empty))))))
                          #:size-of (lambda (inst)
                                      (+ 2 (string-length (instance-value inst)))
                                      )))

(define (size-of inst)
  ((type-size-of (instance-type inst)) inst))

;; (define test-struct-type
;;   ;; struct consisting of an string and an integer
;;   ;;
;;   (make-type
;;    #:size 16
;;    #:read (lambda (in start) #f)
;;    #:write (lambda (val off)
;;              (let-values ((f0.bs f0.tree)
;;              (values
;;               (bytes-append
;;              )))

;;
;; payload needs to be an instance -- an array of values, or a defined struct
;;

(define payload
  `((test-int . ,(instance int32 24))
    (test-string . ,(instance string "this is a test string of utf-8 characters, including a line of Greek: Σὲ γνωρίζω ἀπὸ τὴν κόψη, and some Amharic: ሰማይ አይታረስ ንጉሥ አይከሰስ።. Thanks."))
    (test-word64 . ,(instance word64 #xffffffffffffffff))))

;; header:
;;  magic u4
;;  version i4
;;  size i4
;;  crc32 u4
;; (define (generate-header-bytes bs bs-port)
;;   [bytes-append
;;    #"DC00"
;;    (int32->bytes 1)
;;    (int32->bytes (+ 16 (foldl + 0 (map (compose type-size instance-type) bs))))
;;    (word32->bytes (bytes-crc32 (get-output-bytes bs-port)))
;;    ])

;; (define (write-inst inst port)
;;   ((type-write (instance-type inst)) (instance-value inst) payload-port))

(define (write-bin payload path)
  ;; (map (lambda (pair)
  ;;        (let ((name (car pair))
  ;;              (inst (cdr pair)))
  ;;          (cons (eq-hash-code name)
  ;;                (write-inst inst payload-port))))
  ;;      payload)

  (let ((payload-port (open-output-bytes)))
    (call-with-atomic-output-file
     path
     (lambda (port tmp-path)
       ;;(display (generate-header-bytes payload payload-port) port)

       (display
        [bytes-append
         #"DC00"
         (int32->bytes 1)
         (int32->bytes (for/sum ((inst (map cdr payload))) (size-of inst)))
         (word32->bytes #xcafebabe)
         ] port)

       (for-each
        (lambda (pair)
          (let ((name (car pair))
                (inst (cdr pair)))
            (define-values (bs tree) ((type-write (instance-type inst)) (instance-value inst) 0))
            (display (stitch bs tree) port)
            )
          )
        payload)

       ;;(display (get-output-bytes payload-port) port)
       ;;(printf "bb : ~v~n" bb)
       ;;(display bb port)
       )))
  )

(module* main #f
  ;;[display (equal? 12 (int32-decode (encode-closure int32-encode 12) 0))]

  (write-bin payload (build-path "test.bin"))
  ;;(write (string->bytes/utf-8 (format "test string~n")))

  ;; [write-to-file (string->bytes/utf-8 (format "test string~n"))
  ;;                (build-path "test.bin")
  ;;                #:mode 'binary]
  )

(module+ test
  (require rackunit)

  (test-case "int32"
             (check-equal? (bytes->int32 (int32->bytes 12)) 12)
             (check-equal? (bytes->int32 (int32->bytes -1234567)) -1234567)
             (check-equal? (bytes->int32 (int32->bytes 2147483647)) 2147483647)
             )

  (test-case "word32"
             (check-equal? (bytes->word32 (word32->bytes 12)) 12)
             )

  (test-case "word64"
             (check-equal? (bytes->word64 (word64->bytes #xfedcba9876543210)) #xfedcba9876543210)
             )


  (let ((w (type-write string))
        (r (type-read string)))
    (define-values (bs tree) (w "asdf" 0))
    (display (stitch bs tree))
    )

  )
