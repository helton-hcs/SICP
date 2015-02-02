; ex1.3: Define a procedure that takes three numbers as arguments and returns the sum of the
; squares of the two larger numbers.

#lang racket

(define (sum-of-squared-largest-two x y z)
  (define (square x)
    (* x x))
  (define (sum-of-squares x y)
    (+ (square x) (square y)))
  (cond ((= x (max x y z))
         (sum-of-squares x (max y z)))
        ((= y (max x y z))
         (sum-of-squares y (max x z)))
        (else 
         (sum-of-squares z (max x y)))))
  
; testing

; (sum-of-squared-largest-two 4 5 6) => 5² + 6² = 25 + 36 = 61
(sum-of-squared-largest-two 4 5 6)
(sum-of-squared-largest-two 4 6 5)
(sum-of-squared-largest-two 5 4 6)
(sum-of-squared-largest-two 5 6 4)
(sum-of-squared-largest-two 6 4 5)