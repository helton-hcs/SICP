; ex.1.5: Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is
; using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

#lang racket

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; testing
(test 0 (p))

; the intepreter uses applicative-order because the program never ends (p is infinitely expanded to itself)