#lang scheme

(define (^ base exp)
  (cond ((eq? exp 0) 1)
        ((eq? exp 1) base)
        (else (* base (^ base (- exp 1))))))

(define (calc exp)
  (define operator cadr)
  (define left-operand car)
  (define right-operand caddr)
  (define (apply-operation exp op)
    (cond ((and (number? left-operand) (number? right-operand))
           (op (left-operand exp) (right-operand exp)))
          ((number? left-operand)
           (op (left-operand exp) (calc (right-operand exp))))
          ((number? right-operand)
           (op (calc (left-operand exp)) (right-operand exp)))
          (else
           (op (calc (left-operand exp)) (calc (right-operand exp))))))
  (cond ((number? exp) exp)
        ((eq? (operator exp) '+)
              (apply-operation exp +))
        ((eq? (operator exp) '-)
              (apply-operation exp -))
        ((eq? (operator exp) '*)
              (apply-operation exp *))
        ((eq? (operator exp) '/)
              (apply-operation exp /))
        ((eq? (operator exp) '^)
              (apply-operation exp ^))))


(calc '(1 + 2))
(calc '(2 + (1 + 2)))
(calc '(1 - 2))
(calc '(((6 * 11) * 10) - 2))
(calc '(2 ^ 4))