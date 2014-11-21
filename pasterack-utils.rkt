#lang racket/base
(require racket/date racket/match)
(provide (all-defined-out))

(define o compose)
(define ++ string-append)
(define (to-string d) (format "~a" d))
(define (to-string/v d) (format "~v" d))
(define (to-string/s d) (format "~s" d))

(define (mk-rand-str)
  (number->string (random 100000)))
;  (bytes->string/utf-8 (list->bytes (for/list ([n 4]) (+ 49 (random 9))))))

(define (get-time/iso8601)
  (parameterize ([date-display-format 'iso-8601])
    (match-define (list _ date time)
      (regexp-match
       #px"(\\d\\d\\d\\d-\\d\\d-\\d\\d)[MTWFS](\\d\\d:\\d\\d:\\d\\d)"
       (date->string (current-date) #t)))
    (++ date " " time)))

;; url utils
(define (mk-link url txt) `(a ((href ,url)) ,txt))

;; stx utils
(define (stx->string stx) (to-string/s (syntax->datum stx)))


;; string-truncate : String -> String
;; Truncates the given str to len-limit chars,
;; or returns str unchanged if its length is <= len-limit
(define (string-truncate str len-limit)
  (if (<= (string-length str) len-limit)
      str
      (substring str 0 len-limit)))
