; --------------------
; Definitions
; --------------------

(define (sigma-rec term a next b)
  (if (> a b) 0
    (+ (term a) 
     (sigma-rec term
                (next a)
                next
                b))))

(define (sigma-iter term a next b)
  (define (iter j ans)
    (if (> j b)
      ans
      (iter (next j)
        (+ (term j) ans))))
  (iter a 0))

(define (sum-int-iter a b)
  (define (identity x) x)
  (sigma-iter identity a 1+ b))

(define (sum-int-rec a b)
  (define (identity x) x)
  (sigma-rec identity a 1+ b))

(define (sum-sq-iter a b)
  (sigma-iter square a 1+ b))

(define (sum-sq-rec a b)
  (sigma-rec square a 1+ b))

(define (pi-sum-iter a b)
  (sigma-iter (lambda (i) (/ 1 (* i (+ i 2)))) a (lambda (i) (+ i 4)) b))

(define (pi-sum-rec a b)
  (sigma-rec (lambda (i) (/ 1 (* i (+ i 2)))) a (lambda (i) (+ i 4)) b))

;(define (sum-int a b)
;    (if (> a b) 0
;        (+ a (sum-int (1+ a) b))))

;(define (sum-sq a b)
;    (if (> a b) 0
;        (+ (square a) 
;           (sum-int (1+ a) b))))

;(define (pi-sum a b)
;    (if (> a b) 
;        0
;        (+ (/ 1 (* a (+ a 2)))
;           (pi-sum (+ a 4) b))))

(define (average x y) (/ (+ x y) 2))  

(define (sqrt-1 x)
  (define tolerance 0.00001)
  (define (good-enough? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
      (average (/ x y) y))
  (define (try y)
    (if (good-enough? y)
      y
      (try (improve y))))
  (try 1))

(define (sqrt-2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1))

(define (sqrt-3 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enough? u v) 
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enough? old new) 
      new
      (iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (define (square x) (* x x))
  (newton (lambda (y) (- x (square y))) 1))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point 
    (lambda (x) (- x (/ (f x) (df x)))) guess))

(define (deriv f)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

; --------------------
; Examples
; --------------------

(sum-int-iter 1 100)
(sum-int-rec 1 100)
(sum-sq-iter 1 100)
(sum-sq-rec 1 100)
(pi-sum-iter 1 50)
(pi-sum-rec 1 50)
(sqrt-1 49)
(sqrt-2 49)
(sqrt-3 49)
(sqrt 49)