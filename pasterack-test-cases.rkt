#lang racket/base
;; test cases for pasterack.org
(provide (all-defined-out))

(define test-cases
  '(
    ;; require tests
;    "7262" ; html-render ; deleted for some reason
    "5751" ; only-in
    "9993" ; except-in
    "8521" ; prefix-in
    "6777" ; rename-in
    "4580" ; combine-in
    ;; proper doc linking
    "2425" ; 2htdp/image image? doc link
    "3233" ; htdp/bsl image? doc link
    "6998" ; matrix mult (example of require id, perm, that's also in #lang)
    ;; path permissions
    "7449" ; delete file
    "4749" ; list root
    "8953" ; Sierpinski
    "60761" ; Sierpinski in ASL using recur
    "5563" ; Greek letters
    "4837" ; lazy fib
    "28685" ; set bang (test multi-expr)
    "3259" ; scribble syntax
    "5238" ; big bang (test 2 requires on 1 line)
    "3883" ; echo serv, test limits, and forms in racket but not racket/base
    "7658" ; typed/racket -- also example of begin in top-context
    "9269" ; type error
    "2277" ; checkerboard (slideshow/pict)
    "4786" ; #lang htdp/bsl + 2htdp/image
    "8314" ; check-expect
    "9979" ; check-expect pass
    "96501" ; plot -- also example of begin that should be expression
    "7489" ; bad syntax
    "10731" ; missing #lang
    "79212" ; macro-generated set!
    "4734" ; quibble (module+)
    "5114" ; out of order macros
    "8757" ; out of order defines
    "5795" ; #lang blank
    "4662" ; blank
    "4126" ; nested list of images
    "5791" ; list of images (thanks jrslepak)
    "5568" ; plai
    "29314"; fish pict
    "12143" ; comment before #lang
    "70309" ; s-exp comment before #lang
    "4683" ; explicit reader
    "32200" ; at-exp
    "58952" ; 2d
    "11302" ; issue #61 stx->list
    ;; BROKEN: submodule evaluation
    ))
