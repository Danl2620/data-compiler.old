#lang racket/base

(require
 racket/bytes
 "integer.rkt"
 "types.rkt"
  "crc32.rkt"
  "encode.rkt"
 )

(provide make-module-bytes)

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

(define (make-module-bytes payload)
  (let ((header-bytes (generate-header->bytes payload))
        (payload-bytes (bytes-append
                        ;; write name/offsets, then items
                        (let* ((bss (map (compose encode cdr) payload))
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
    (bytes-append header-bytes payload-bytes)
    ))
