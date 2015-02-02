#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)))
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
  
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else 
         (cons (car set1)
               (union-set (cdr set1) set2)))))
  
(define (order set)
  (cond ((null? set) '())
        ((null? (cdr set)) (car set))
        (else
         (if (< (car set) (cadr set))
             (cons (car set) (order (cdr set)))
             (cons (cadr set) (order (cons (car set) (cddr set))))))))
   
; testing

;(element-of-set? 'a '(1 2 a b))
;(adjoin-set 'a '(1 2 a b))
;(adjoin-set 'a '(1 2 c b))
;(intersection-set '(1 2 3) '(a 1 b 3))
;(intersection-set '(1 2 3) '(2 3 4))
;(union-set '(1 2 3) '(2 3 4))
;(union-set '(4 1 3 2) '(2 6 3 4 8))
(order '(5 2 4))