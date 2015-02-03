#lang scheme

; --------------------
; Table operations
; --------------------
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; --------------------
; Data type operations
; --------------------

(define (attach-type type contents)
  (cons type contents))

(define type car)
(define contents cdr)

; --------------------
; Generic operators
; --------------------
(define (apply-generic op obj)
  (let ((proc (get (type obj) op))
        (args (contents obj)))
    (if (not (null? proc))
        (proc args)
        (error "Undefined operator"))))

(define (apply-generic-2 op arg1 arg2)
  (if
   (eq? (type arg1) (type arg2))
   (let ((proc (get (type arg1) op))
         (arg1_args (contents arg1))
         (arg2_args (contents arg2)))
     (if (not (null? proc))
         (proc arg1_args arg2_args)
         (error "Operation undefined on type")))
   (begin
     (display (type arg1))
     (display (type arg2))
     (error "Args not same type"))))

; --------------------
; Complex numbers - Rectangular and Polar representations
; --------------------

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

; selectors (using generic operators)

(define (real-part obj)
  (apply-generic 'real-part obj))

(define (imag-part obj)
  (apply-generic 'imag-part obj))

(define (magnitude obj)
  (apply-generic 'magnitude obj))

(define (angle obj)
  (apply-generic 'angle obj))

; installing the <complex-rectangular> package

(put 'rectangular 'real-part real-part-rectangular)
(put 'rectangular 'imag-part imag-part-rectangular)
(put 'rectangular 'magnitude magnitude-rectangular)
(put 'rectangular 'angle     angle-rectangular)

; installing the <complex-polar> package

(put 'polar 'real-part real-part-polar)
(put 'polar 'imag-part imag-part-polar)
(put 'polar 'magnitude magnitude-polar)
(put 'polar 'angle     angle-polar)

; --------------------
; Complex numbers
; --------------------

; constructor

(define (make-complex z)
  (attach-type 'complex z))

; operators

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

; printer

(define (print-complex z)
  (define (print-real-part z)
    (display (real-part z)))
  (define (print-imag-part z)
    (display (imag-part z))
    (display "i"))
  (display "{Complex:")
  (cond ((zero? (imag-part z))
         (print-real-part z))
        ((zero? (real-part z))
         (print-imag-part z))
        (else
         (print-real-part z)
         (cond ((negative? (imag-part z))
                (display " ")
                (print-imag-part z))
               (else
                (display " + ")
                (print-imag-part z)))))
  (display " [")
  (display (type z))
  (display "-notation]}"))

; installing <complex numbers> package

(put 'complex 'add   +complex)
(put 'complex 'sub   -complex)
(put 'complex 'mul   *complex)
(put 'complex 'div   /complex)
(put 'complex 'print print-complex)

; --------------------
; Rational numbers
; --------------------

; constructor

(define (make-rat n d)
  (attach-type 
   'rational 
   (let ((g (gcd n d)))
     (cons (/ n g) 
           (/ d g)))))

; selectors

(define numer car)
(define denom cdr)

; operators

(define (+rat x y)
  (make-rat 
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (-rat x y)
  (make-rat 
   (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat 
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (/rat x y)
  (make-rat 
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

; printer

(define (print-rat x)
  (display "{Rational:")
  (display (numer x)) (display "/") (display (denom x))
  (display "}"))

; installing <rational numbers> package

(put 'rational 'add   +rat)
(put 'rational 'sub   -rat)
(put 'rational 'mul   *rat)
(put 'rational 'div   /rat)
(put 'rational 'print print-rat)

; --------------------
; Ordinary Numbers
; --------------------

; constructor

(define (make-number n)
  (attach-type 'number n))

; operators

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

; printer

(define (print-number x)
  (display "{Number:") (display x) (display "}"))

; installing <number> package

(put 'number 'add   +number)
(put 'number 'sub   -number)
(put 'number 'mul   *number)
(put 'number 'div   /number)
(put 'number 'print print-number)

; --------------------
; Polynomials
; --------------------

; constructors

(define (make-poly variable term-list)
  (attach-type 
   'polynomial
   (cons variable term-list)))

(define (the-empty-termlist) '())

; selectors

(define variable car)

(define term-list cdr)

(define (first-term term-list) term-list)

(define rest-terms cdr)

(define (order term) (- (length term) 1))

(define coeff car)

; predicates

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (empty-termlist? term-list) (null? term-list))

; operators

(define (adjoin-term term term-list)
  (cons term term-list))

(define (negate poly)
  (define (iter terms result)
    (if (null? terms)
        (make-poly (variable poly) result)
        (let* ((term (car terms))
               (negated (apply-generic 'negate term)))
          (iter (cdr terms) (append result (list negated))))))
  (iter (term-list poly) '()))

(define (add-terms terms1 terms2)
  (let ((t1 (first-term terms1)) (t2 (first-term terms2)))
    (cond ((empty-termlist? terms1) terms2)
          ((empty-termlist? terms2) terms1)
          ((> (length terms1) (length terms2))
           (adjoin-term (coeff t1)
                        (add-terms (rest-terms terms1) terms2)))
          ((< (length terms1) (length terms2))
           (adjoin-term (coeff t2)
                        (add-terms terms1 (rest-terms terms2))))
          (else
           (adjoin-term
            (add (coeff t1) (coeff t2))
            (add-terms (rest-terms terms1)
                       (rest-terms terms2)))))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials contain different variables -- ADD-POLY" (list p1 p2))))

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (negate p2))
      (error "Polynomials contain different variables -- SUB-POLY" (list p1 p2))))

(define (mul-terms terms1 terms2)
  (define (mul-term-by-all-terms term terms)
    (define (n-zeros n)
      (if (<= n 0)
          '()
          (cons 0 (n-zeros (- n 1)))))
    (define (compute-coeffs terms)
      (if (empty-termlist? terms)
          (the-empty-termlist)
          (let ((term2 (first-term terms)))
            (adjoin-term
             (mul (coeff term) (coeff term2))
             (compute-coeffs (rest-terms terms))))))
    (append (compute-coeffs terms) (n-zeros (order term))))
  (if (empty-termlist? terms1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term terms1) terms2)
                 (mul-terms (rest-terms terms1) terms2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials contain different variables -- MUL-POLY" (list p1 p2))))

; printer

(define (print-poly p)
  (define (print-term term)
    (let ((c (cadr term))
          (o (car  term)))
      (cond ((not (eq? c 1))
             (display c)))
      (cond ((eq? o 0)
             (display ""))
            ((eq? o 1)
             (display (variable p)))
            (else 
             (display (variable p)) (display "^") (display o)))))
  (define (print-terms term-list)
    (print-term (car term-list))
    (cond ((not (null? (cdr term-list)))
           (display " + ")
           (print-terms (cdr term-list)))))
  (display "{Polynomial:")
  (print-terms (term-list p))
  (display "}"))

; installing <polynomial> package
(put 'polynomial 'add   add-poly)
(put 'polynomial 'sub   sub-poly)
(put 'polynomial 'mul   mul-poly)
;(put 'polynomial 'div   /poly)
(put 'polynomial 'print print-poly)

; --------------------
; Generic Arithmetic System
; --------------------

(define (add x y)
  (apply-generic-2 'add x y))

(define (sub x y)
  (apply-generic-2 'sub x y))

(define (mul x y)
  (apply-generic-2 'mul x y))

(define (div x y)
  (apply-generic-2 'div x y))

(define (print obj)
  (apply-generic 'print obj)
  (newline))

; --------------------
; Testing
; --------------------

(print (make-number 1))
(print (add (make-number 4) (make-number 6)))
(print (sub (make-number 4) (make-number 6)))
(print (mul (make-number 4) (make-number 6)))
(print (div (make-number 4) (make-number 6)))

(print (make-rat 5 9))
(print (add (make-rat 1 2) (make-rat 1 4)))
(print (sub (make-rat 1 2) (make-rat 1 4)))
(print (mul (make-rat 1 2) (make-rat 1 4)))
(print (div (make-rat 1 2) (make-rat 1 4)))

(print (make-complex (make-rectangular 0 5)))
(print (make-complex (make-polar 3 4)))
(print (add (make-complex (make-rectangular 1 2)) (make-complex (make-polar 2 3))))
(print (sub (make-complex (make-rectangular 1 2)) (make-complex (make-polar 2 3))))
(print (mul (make-complex (make-rectangular 1 2)) (make-complex (make-polar 2 3))))
(print (div (make-complex (make-rectangular 1 2)) (make-complex (make-polar 2 3))))

(print (make-poly 'x '((2 1) (1 2) (0 -1)))) ; 1*x^2 + 2*x^1 - 1*x^0 = x^2 + 2x - 1
(print (make-poly 'y '((6 7) (5 3) (1 2) (2 1))))
;(print (add 
;        (make-poly 'z '((6 7) (5 3) (4 0) (3 0) (2 1) (1 2) (0 4)))
;        (make-poly 'z '((6 3) (5 0) (4 0) (3 1) (2 3) (1 0) (0 0)))))