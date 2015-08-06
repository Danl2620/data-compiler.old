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
 "types.rkt"
 "integer.rkt"
 "crc32.rkt"
 "params.rkt"
 ]



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
;; payload is a alist of name, instance pairs
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

(define (generate-header->bytes payload)
  [bytes-append
   #"Frac"
   (int32->bytes 1)
   (int32->bytes (for/sum ((inst (map cdr payload))) (size-of inst)))
   (word32->bytes #xcafebabe)
   ])

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

(define (write-bin payload path)
  (let ((header-bytes (generate-header->bytes payload))
        (payload-bytes (bytes-append
                        ;; write name/offsets, then items
                        (let* ((bss (map
                                     (lambda (pair)
                                       (let ((inst (cdr pair)))
                                         (let-values [((bs tree) ((type-write (instance-type inst)) (instance-value inst) 0))]
                                           (stitch bs tree))))
                                     payload))
                               (lens (map bytes-length bss))
                               (offset 0)
                               (offsets (map (lambda (off)
                                               (let ((v (+ offset off)))
                                                 (set! offset (+ offset off))
                                                 v))
                                             lens)))

                          (bytes-append
                           ;; write name/offset pairs
                           (bytes-append*
                            (map
                             (lambda (pair off)
                               (let ((name (car pair))
                                     (inst (cdr pair)))
                                 (bytes-append
                                  (word32->bytes (crc32/string (symbol->string name)))
                                  (word32->bytes off)
                                  )))
                             payload
                             offsets))

                           ;; write items
                           (bytes-append* bss))
                          ))))
    (call-with-atomic-output-file
     path
     (lambda (port tmp-path)
       (display header-bytes port)
       (display payload-bytes port)
       ))))

(module* main #f
  (write-bin payload (build-path "test.bin"))
  )

(module+ test
  (require rackunit)
  )
