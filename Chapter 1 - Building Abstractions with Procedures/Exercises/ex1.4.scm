; ex1.4: Observe that our model of evaluation allows for combinations whose operators are
; compound expressions. Use this observation to describe the behavior of the following procedure:

#lang racket

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
  
; testing
(a-plus-abs-b  3  4)
(a-plus-abs-b  3 -4)
(a-plus-abs-b -3  4)
(a-plus-abs-b -3 -4)