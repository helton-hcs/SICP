#lang scheme

; incomplete!

(define (deriv exp var)
  (define sum-symbol '+)
  (define times-symbol '*)
  (define exponentiation-symbol '^)
  (define variable? symbol?)
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define operator car)
  (define left-operand cadr)
  (define right-operand caddr)
  (define (build-operation operator left right)
    (list operator left right))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (build-operation sum-symbol a1 a2))))
  (define (sum? x)
    (eq? (operator x) sum-symbol))
  (define addend left-operand)
  (define augend right-operand)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (build-operation times-symbol m1 m2))))
  (define (product? x)
    (eq? (operator x) times-symbol))
  (define multiplier left-operand)
  (define multiplicand right-operand)
  (define (exponentiation? x)
    (eq? (operator x) exponentiation-symbol))
  (define base left-operand)
  (define exponent right-operand)
  (define (make-exponentiation b e)
    (cond ((and (number? e) (eq? e 0)) 1)
          ((and (number? e) (eq? e 1)) b)
          (else (build-operation exponentiation-symbol b e))))
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (- (exponent exp) 1)))
          (deriv (base exp) var)))         
        (else 
         (error "unknown expression type -- DERIV" exp))))

; testing

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(^ x 2) 'x)
(deriv '(^ x 3) 'x)
(deriv '(* (^ x 3) 2) 'x)
;(deriv '(+ x x 4 6 7) 'x)
;(deriv '(* x x 4 6 7) 'x)