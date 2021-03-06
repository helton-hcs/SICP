; --------------------
; Definitions
; --------------------

; representing cons, car and cdr (already built-in Scheme)

(define (_cons a b)
  (lambda (index)
    (cond ((= index 1) a)
          ((= index 2) b))))

(define (_car x) (x 1))

(define (_cdr x) (x 2))

; representing rational numbers

(define (make-rat n d)
  (let ((g (gcd n d)))
    (_cons (/ n g) 
          (/ d g))))

(define (numer x)
  (_car x))

(define (denom x)
  (_cdr x))

(define (+rat x y)
  (make-rat 
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat 
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (print-rat x)
  (display "{Rational:")
  (display (numer x)) (display "/") (display (denom x))
  (display "}"))

; representing vector in the plane

(define (make-vector x y)
  (_cons x y))

(define (xcor p)
  (_car p))

(define (ycor p)
  (_cdr p))

(define (print-vector v)
  (display "{Vector:(")
  (display (xcor v)) (display ",") (display (ycor v))
  (display ")}"))

; representing line segments

(define (make-seg p q)
  (_cons p q))

(define (seg-start s)
  (_car s))

(define (seg-end s)
  (_cdr s))

(define (print-seg s)
  (display "{Segment:")
  (print-vector (seg-start s)) (display "=>") (print-vector (seg-end s))
  (display "}"))

(define (midpoint-seg s)
  (define (average x y) (/ (+ x y) 2))
  (let ((a (seg-start s))
        (b (seg-end s)))
       (make-vector 
        (average (xcor a) (xcor b))
        (average (ycor a) (ycor b)))))

(define (length-seg s)
  (let 
    ((dx (- (xcor (seg-end s))
            (xcor (seg-start s))))
     (dy (- (ycor (seg-end s))
            (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

; --------------------
; Examples
; --------------------

(define a (make-rat 1 2))
(define b (make-rat 1 4))
(define ans (+rat a b))
(print-rat ans)

(define p (make-vector 2 3))
(define q (make-vector 5 1))
(print-vector p)
(print-vector q)

(define s1 (make-seg p q))
(print-seg s1)

(define mid (midpoint-seg s1))
(print-vector mid)

(define l (length-seg s1))
(display l)