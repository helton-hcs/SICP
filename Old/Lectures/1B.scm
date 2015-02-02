; --------------------
; Definitions
; --------------------

(define (sqrt x)
    (define (average x y) (/ (+ x y) 2))    
    (define (improve guess)
        (average guess (/ x guess)))
    (define (good-enough? guess)
        (< (abs (- (square guess) x))
            .001))
    (define (try guess)
        (if (good-enough? guess)
            guess
            (try (improve guess))))
    (try 1))

; sum

(define (sum-two-1 x y) 
    (if (= x 0)
        y
        (sum-two-1 (-1+ x) (1+ y))))

(define (sum-two-2 x y) 
    (if (= x 0)
        y
        (1+ (sum-two-2 (-1+ x) y))))

; fib

(define (fib n)
    (if (< n 2)
        n
        (+  (fib (- n 1))
            (fib (- n 2)))))

; hanoi towers

(define (print-move from to)
    (newline)
    (display from)
    (display " => ")
    (display to))   

(define (move n from to spare)
    (cond 
        ((= n 0) "Done")
        (else 
            (move (-1+ n) from spare to)
            (print-move from to)
            (move (-1+ n) spare to from))))

; --------------------
; Examples
; --------------------

(sqrt 49)
(sum-two-1 4 -5)
(sum-two-1 15 88)
(sum-two-2 4 -5)
(sum-two-2 15 88)
(fib 10)
(move 4 1 2 3)