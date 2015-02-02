;#lang planet neil/sicp
#lang racket

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

; vector

(define (make-vector x y)
  (cons x y))

(define (vector-xcor p)
  (car p))

(define (vector-ycor p)
  (cdr p))

(define (+vector v1 v2)
  (make-vector
   (+ (vector-xcor v1) (vector-xcor v2))
   (+ (vector-ycor v1) (vector-ycor v2))))

(define (scale-vector s v)
  (make-vector
   (* s (vector-xcor v))
   (* s (vector-ycor v))))

(define (print-vector v)
  (display "{Vector:(")
  (display (vector-xcor v)) (display ",") (display (vector-ycor v))
  (display ")}"))

; segment

(define (make-segment p q)
  (cons p q))

(define (segment-start s)
  (car s))

(define (segment-end s)
  (cdr s))

(define (print-segment s)
  (display "{Segment:")
  (print-vector (segment-start s)) (display "=>") (print-vector (segment-end s))
  (display "}"))

; rectangle

(define (make-rectangle origin horizon vert)
  (list origin horizon vert))

(define (rectangle-origin rectangle)
  (car rectangle))

(define (rectangle-horizon rectangle)
  (car (cdr rectangle)))

(define (rectangle-vert rectangle)
  (car (cdr (cdr rectangle))))

(define (print-rectangle rectangle)
  (display "{Rectangle:")
  (display "[Origin:") (print-vector (rectangle-origin rectangle)) (display "],")
  (display "[Horizon:") (print-segment (rectangle-horizon rectangle)) (display "],")
  (display "[Vert:") (print-segment (rectangle-vert rectangle)) (display "]")
  (display "}"))

(define (coord-map rectangle)
  (lambda (point)
    (+vector 
     (+vector (scale-vector (vector-xcor point)
                            (rectangle-horizon rectangle))
              (scale-vector (vector-ycor point)
                            (rectangle-vert rectangle)))
     (rectangle-origin rectangle))))

; picture

(define (make-picture seglist)
  (lambda (rectangle)
    (for-each
     (lambda (s)
       (line
        ((coord-map rectangle) (segment-start s))
        ((coord-map rectangle) (segment-end s))))
     seglist)))

; usage

(define v1 (make-vector 0 0))
(define v2 (make-vector 0 10))
(define v3 (make-vector 10 10))
(define h (make-segment v1 v3))
(define v (make-segment v1 v2))
(define r (make-rectangle v1 h v))
(print-rectangle r)

(define g (make-picture 
           (list (make-segment 
                  (make-vector 1 2)
                  (make-vector 5 2))
                 (make-segment
                  (make-vector 2 1)
                  (make-vector 2 5)))))
(g r)