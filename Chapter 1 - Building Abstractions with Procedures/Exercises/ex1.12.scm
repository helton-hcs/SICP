; ex1.12: The following pattern of numbers is called Pascal's triangle.
;
;         1
;       1   1
;     1   1   1
;   1   3   3   1
; 1   4   6   4   1
;        ... 
;
; 1
; 1 1
; 1 1 1
; 1 3 3 1
; 1 4 6 4 1
;

; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two
; numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive
; process.

#lang scheme

(define (pascal-triangle max-depth)
  (define (pascal-triangle-helper row column max-depth)
    (define (pascal row column)
      (define (is-out-of-bounds?) 
        (define (is-value-out-of-bounds? n) (<= n 0))
        (or (is-value-out-of-bounds? row) 
            (is-value-out-of-bounds? column)))
      (define (is-edge?) 
        (or (= column 1)
            (= row column)))
      (define (left-parent) 
        (pascal (- row 1) (- column 1)))
      (define (right-parent)
        (pascal (- row 1) column)) 
      (cond ((is-edge?) 1)
            ((is-out-of-bounds?) 0)       
            (else (+ (left-parent) 
                     (right-parent)))))
    (define (should-print?)
      (and (<= row max-depth) (<= column row)))
    (define (print-info row column)
      (display (pascal row column)) (display " "))
    (cond ((should-print?)
           (print-info row column)
           (cond ((= row column)
                  (newline)
                  (pascal-triangle-helper (+ row 1) 1 max-depth))
                 (else 
                  (pascal-triangle-helper row (+ column 1) max-depth))))))  
  (pascal-triangle-helper 1 1 max-depth))

(pascal-triangle 25)