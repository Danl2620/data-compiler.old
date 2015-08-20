#lang racket/base

[require
 (for-syntax racket/base
             syntax/parse
             racket/syntax
             racket/list)
 racket/file
 racket/list
 "integer.rkt"
 "types.rkt"
 "module.rkt"
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

(define (write-bin payload path)
  (call-with-atomic-output-file
   path
   (lambda (port tmp-path)
     (display (make-module-bytes payload) port)
     )))

(module* main #f
  (write-bin payload (build-path "stream-test.bin"))
  )
