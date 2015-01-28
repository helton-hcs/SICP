; --------------------
; Definitions
; --------------------

(define (square x) (* x x))

(define sqr (lambda (x) (* x x)))

(define (average x y) (/ (+ x y) 2))

(define (means-square x y)
  (average
    (square x) 
    (square y)))

(define (absolute x)
    (cond 
        ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

(define (abs x) 
    (if (< x 0)
        (- x)
    x))

; --------------------
; Examples
; --------------------

(square 10)
(sqr 6)
(average 4 5)
(means-square 6 7)
(absolute 10)
(absolute -20.3)
(absolute 0)
(abs 10)
(abs -20.3)
(abs 0)