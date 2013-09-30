#lang scribble/manual
@(require scribble/eval)
@(require (for-label racket))
@(define the-eval (make-base-eval))
@codeblock{
  (+ 1 2)
  (+ 3 4)}
@interaction-eval-show[#:eval the-eval \#lang racket]
