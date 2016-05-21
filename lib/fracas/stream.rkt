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

(define *test-string* "this is a string of utf-8 characters, including a line of Greek: Σὲ γνωρίζω ἀπὸ τὴν κόψη, and some Amharic: ሰማይ አይታረስ ንጉሥ አይከሰስ።. Thanks.")

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


(define test-module
  (fracas-module
	  (vector 'test-int 'test-string 'test-word64 'test-pint32)
	  (vector (instance int32 24)
			  (instance string *test-string*)
			  (instance word64 #xffffffffffffffff)
			  (instance (pointer int32) 48)
			  )))

(define (write-bin mod path)
  (call-with-atomic-output-file
   path
   (lambda (port tmp-path)
     (display (make-module-bytes mod) port)
     )))

(module* main #f
  (write-bin test-module (build-path "test.bin"))
  )
