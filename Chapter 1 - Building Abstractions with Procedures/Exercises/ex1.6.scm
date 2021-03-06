; ex1.6: Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I
; just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims
; this can indeed be done, and she defines a new version of if:

#lang racket

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt x)
  (define (sqrt-iter guess x)
    (define (good-enough? guess x)
      (define (square x) (* x x))
      (< (abs (- (square guess) x)) .001))
    (define (improve guess x)
      (define (average x y)
        (/ (+ x y) 2))
      (average guess (/ x guess)))
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (sqrt-iter 1.0 x))

; Eva demonstrates the program for Alyssa:
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(sqrt 36)
(sqrt 49)

; Delighted, Alyssa uses new-if to rewrite the square-root program:


; What happens when Alyssa attempts to use this to compute square roots? Explain.
; R: It never ends! Because Lisp is do a applicative-order, what means that the then-clause and else-clause are always evaluated before passed to new-if.