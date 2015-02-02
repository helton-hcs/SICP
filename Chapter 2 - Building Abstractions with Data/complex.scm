#lang scheme

; type data

(define (attach-type type contents)
  (cons type contents))

(define type car)
(define contents cdr)

; predicates

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

; operators

(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

; constructors

(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

; selectors

(define real-part-rectangular car)

(define (real-part-polar z)
  (* (car z) (cos (cdr z))))

(define imag-part-rectangular cdr)

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-rectangular z)
  (define (square x) (* x x))
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define magnitude-polar car)

(define (angle-rectangular z)
  (atan (cdr z) (car z)))
  
(define angle-polar cdr)

; selectors - generic operators

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular
          (contents z)))
        ((polar? z)
         (real-part-polar
          (contents z)))))
          
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular
          (contents z)))
        ((polar? z)
         (imag-part-polar
          (contents z)))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular
          (contents z)))
        ((polar? z)
         (magnitude-polar
          (contents z)))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular
          (contents z)))
        ((polar? z)
         (angle-polar
          (contents z)))))

; printers

(define (print-complex z)
  (define (print-real-part z)
    (display (real-part z)))
  (define (print-imag-part z)
    (display (imag-part z))
    (display "i"))
  (cond ((zero? (imag-part z))
         (print-real-part z))
        ((zero? (real-part z))
         (print-imag-part z))
        (else
         (print-real-part z)
         (display " + ")
         (print-imag-part z)))
  (newline))

; testing

(define c1 (make-rectangular 0 5))
(define c2 (make-rectangular 3 0))
(define c3 (make-rectangular 3 5))
(print-complex c1)
(print-complex c2)
(print-complex c3)