; ex1.8: Newton's method for cube roots is based on the fact that if y is an approximation to the cube
; root of x, then a better approximation is given by the value:
; (x/y² + 2y) / 3
; Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In
; section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these square-
; root and cube-root procedures.)

#lang racket

(define (cbrt x)
  (define (cbrt-iter guess x)
    (define (square x) (* x x))
    (define (good-enough? guess x)
      (define (cube x) (* x (square x)))
      (< (abs (- x (cube guess))) .001))
    (define (improve guess x)
      (define (average x y)
        (/ (+ x y) 2))
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (if (good-enough? guess x)
        guess
        (cbrt-iter (improve guess x) x)))
  (cbrt-iter 1.0 x))
  
(cbrt 125)