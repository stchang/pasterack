#lang racket (require xml)

(define xe (xml->xexpr (document-element (with-input-from-file "test.html" read-xml))))

(require xml/path)

(define lst (car (filter
 (λ (div)
   (equal? "main" (se-path* '(div #:class) div)))
 (se-path*/list '(div) xe))))
lst
;(define paras (cdddr lst))
;(cons (car lst) (cons (cadr lst)
;                      (cdddr lst)))

;(match lst
;  [`(