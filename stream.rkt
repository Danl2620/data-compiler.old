#lang racket/base

[require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/list)
         racket/file
         racket/match
         ]

[define empty '[]]

;; encode : value -> bytes
;; decode : bytes int -> value

;; --->

;; encode : (v:value) ->
;;       (vbs:bytes) (list (pairs points-from encode-thunk))
;; decode : (heap:bytes) (start:addr)           -> value

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
     (integer->integer-bytes start 4 #f #t nbs addr)
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

(define (encode-closure encode v)
  (define-values (fbs post-tree) (encode v 0))
  (stitch fbs post-tree))

(define-syntax branch*
  (syntax-rules ()
    [(_) empty]
    [(_ o) o]
    [(_ o t) (branch o t)]
    [(_ o t ...) (branch o (branch* t ...))]))

(begin-for-syntax
  (define-syntax-class spec
    #:attributes (encode decode size)

	[pattern
     ((~literal struct) field0:spec ...
      (~optional (~seq #:align align-e:expr)))

     #:with ((e.start e.end) ...)
	 (let ()
	   (define-values (final-end e.start-in-reverse)
		 (for/fold ([last-end 0]
					[ans empty])
			 ([f.size (in-list (attribute field0.size))])
		   (define my-start last-end)
		   (define my-end (+ my-start f.size))
		   (values my-end
				   (list* (list my-start
								my-end)
						  ans))))
	   (datum->syntax #'here (reverse e.start-in-reverse)))

	 #:attr size
     (apply + (attribute field0.size))
     #:attr encode
     (with-syntax
         ([((i e.bs e.tree) ...)
           (for/list ([i (in-naturals)]
                      [e (in-list (syntax->list #'(field0 ...)))])
             (list i
                   (generate-temporary)
                   (generate-temporary)))])
       #'(λ (v off)
           (let-values ([(e.bs e.tree)
                         (field0.encode (vector-ref v i) (+ off e.start))]
                        ...)
             (values
              (bytes-append
               e.bs
               ...)
              ;; e.tree contains 0 everywhere but should be offset by
              ;; e.start either do that here or later (in stitch)
              ;; OR
              ;; heap place-to-write value -> void
              (branch* e.tree ...)))))
     #:attr decode
       #'(λ (bs start)
           (vector (field0.decode bs (+ start e.start))
                   ...))]

    [pattern
     ((~literal pointer) type-s:spec)
     #:attr size 4
     #:attr encode
     #`(λ (v off)
         (values (integer->integer-bytes #xffffffff 4 #f #t)
                 (list (addr*thunk
                        off
                        (λ (thunk-off)
						   (type-s.encode v thunk-off)
						   )))))
     #:attr decode
     #'(λ (bs start)
		  (let ((offset (integer-bytes->integer bs #f #t start (+ start 4))))
			(type-s.decode bs offset)))]

	;; [pattern
	;;  (~literal string)
	;;  #:attr size 0
	;;  #:attr encode
	;;  #'(λ (v off) (values (integer->integer-bytes #xffffffff 4 #f #t)
	;; 					  (list (addr*thunk
	;; 							 off
	;; 							 (λ (thunk-off)
	;; 								(bytes-append
	;; 								 (integer->integer-bytes (string-length v) 2 #f #t)
	;; 								 (string->bytes/utf-8 v)))))))
	;;  #:attr decode
	;;  #'(λ (bs start)
	;; 	  (let* ((offset (integer-bytes->integer bs #f #t start (+ start 4)))
	;; 			 (len (integer-bytes->integer bs #f #t offset (+ offset 2))))
	;; 		(bytes->string/utf-8 bs #f (+ offset 2) (+ offset 2 len))))]

	[pattern
     other-s
     #:declare other-s (static spec-info? "spec")
     #:attr size
     (spec-info-size (attribute other-s.value))
     #:attr encode
     (spec-info-encode-id (attribute other-s.value))
     #:attr decode
     (spec-info-decode-id (attribute other-s.value))]

	))

(begin-for-syntax
  (struct spec-info (encode-id decode-id size)))

(define-syntax #%int32
  [spec-info #'(λ (v off) (values (integer->integer-bytes v 4 #f #t) empty))
			 #'(λ (bs start) (integer-bytes->integer bs #f #t start (+ start 4)))
			 4])

(define-syntax (define-spec stx)
  (syntax-parse stx
    [(_ name:id s:spec)
	 (with-syntax
	  ([(encode decode)
		(let ((new-name #'name))
		  (list (format-id new-name "~a-encode" new-name)
				(format-id new-name "~a-decode" new-name)))])

     (quasisyntax/loc stx
       (begin
         (define-syntax name
           (spec-info #'encode #'decode
                      #,(attribute s.size)))
         (define encode s.encode)
         (define decode s.decode))))]))

(define-spec int32 #%int32)


(define (main)
  [display (equal? 12 (int32-decode (encode-closure int32-encode 12) 0))]

  (call-with-atomic-output-file
   (build-path "test.bin")
   (lambda (port tmp-path)

     (let ((bs [bytes-append
                #"DC00"
                (integer->integer-bytes 12 4 #t)
                (real->floating-point-bytes 12.34 4)
                ]))

       ;;(string->bytes/utf-8 (format "test string~n"))
       (display bs port))
     ))

  ;;(write (string->bytes/utf-8 (format "test string~n")))

  ;; [write-to-file (string->bytes/utf-8 (format "test string~n"))
  ;;                (build-path "test.bin")
  ;;                #:mode 'binary]
  )

(main)
