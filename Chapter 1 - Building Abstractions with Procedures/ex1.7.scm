; ex.1.7: The good-enough? test used in computing square roots will not be very effective for
; finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost
; always performed with limited precision. This makes our test inadequate for very large numbers. Explain
; these statements, with examples showing how the test fails for small and large numbers. An alternative
; strategy for implementing good-enough? is to watch how guess changes from one iteration to the
; next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that
; uses this kind of end test. Does this work better for small and large numbers?

#lang racket

(define (sqrt x)
  (define (sqrt-iter guess last-guess x)
    (define (good-enough? guess last-guess x)
      (define (square x) (* x x))
      (< (abs (- last-guess guess)) .001))
    (define (improve guess x)
      (define (average x y)
        (/ (+ x y) 2))
      (average guess (/ x guess)))
    (if (good-enough? guess last-guess x)
        guess
        (sqrt-iter (improve guess x) guess x)))
  (sqrt-iter 1.0 0.0 x))

(sqrt 9876543)
(sqrt 49)