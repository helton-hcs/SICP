; --------------------
; Definitions
; --------------------

; representing cons, car and cdr (already built-in Scheme)

(define (_cons x y)
  (lambda (index)
    (cond ((= index 1) x)
          ((= index 2) y))))

(define (_car x)
  (x 1))

(define (_cdr x)
  (x 2))

; representing vector in the plane

(define make-vector _cons)

(define (xcor p) (_car p))

(define (ycor p) (_cdr p))

(define (print-vector v)
  (display "{Vector:(")
  (display (xcor v)) (display ",") (display (ycor v))
  (display ")}"))

(define (+vector v1 v2)
  (make-vector
    (+ (xcor v1) (xcor v2))
    (+ (ycor v1) (ycor v2))))

(define (scale-vector s v)
  (make-vector
    (* s (xcor v))
    (* s (ycor v))))

; functions already built-in Scheme

(define (_map p l)
  (define nil '())
  (if (null? l)
      nil
      (cons (p (car l)) 
            (_map p (cdr l)))))

(define (_for-each proc l)
  (if (not (null? l))
      (begin
        (proc (car l))
        (_for-each proc 
                   (cdr l)))))

; using the custom functions

(define (scale-list s l)
  (_map (lambda (item) (* item s)) l))

(define (print-list l)
  (_for-each (lambda (item) 
                (newline)
                (display item)) 
             l))

; --------------------
; Examples
; --------------------
(define v1 (make-vector 1 2))
(define v2 (make-vector 2 3))
(define v3 (+vector v1 v2))
(print-vector v3)
(define sv1 (scale-vector 10 v1))
(print-vector sv1)

(define 1-to-4 (list 1 2 3 4))
(display 1-to-4)
(car (cdr 1-to-4))
(car (cdr (cdr 1-to-4)))

(scale-list 10 1-to-4)
(print-list 1-to-4)