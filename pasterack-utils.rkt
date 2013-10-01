#lang racket/base
(provide (all-defined-out))

(define ++ string-append)

(define (mk-rand-str)
  (bytes->string/utf-8 (list->bytes (for/list ([n 4]) (+ 49 (random 9))))))
