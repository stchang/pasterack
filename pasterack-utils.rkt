#lang racket/base
(require racket/date racket/match)
(provide (all-defined-out))

(define ++ string-append)
(define (to-string d) (format "~a" d))

(define (mk-rand-str)
  (bytes->string/utf-8 (list->bytes (for/list ([n 4]) (+ 49 (random 9))))))

(define (get-time/iso8601)
  (parameterize ([date-display-format 'iso-8601])
    (match-define (list _ date time)
      (regexp-match
       #px"(\\d\\d\\d\\d-\\d\\d-\\d\\d)[MTWFS](\\d\\d:\\d\\d:\\d\\d)"
       (date->string (current-date) #t)))
    (++ date " " time)))
