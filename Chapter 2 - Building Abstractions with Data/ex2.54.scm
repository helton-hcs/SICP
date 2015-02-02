#lang scheme

(define (equal? l1 l2)
  (cond ((and (null? l1)
              (null? l2))
         #t)
        ((or (and (null? l1)
                  (not (null? l2)))
             (and (null? l2)
                  (not (null? l1))))
         #f)
        (else 
         (and 
          (eq? (car l1) (car l2))
          (equal? (cdr l1) (cdr l2))))))

; testing

(equal? '(1 2 3)   '(1 2 3))
(equal? '(1 2 3)   '(1.0 2.0 3.0))
(equal? '(1 2 3)   '(1 2 3 4))
(equal? '(1 2 3 4) '(1 2 3))
(equal? '(1 2 3) '())
(equal? '() '(1 2 3))
