; constants
(define empty-stream '())
  
; stream library
(define (memoize procedure)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (procedure))
                 (set! already-run? #t)
                 result)
          result))))
  
(define (delay expression)
  (memoize (lambda () expression)))
  
(define (force delayed-object)
  (delayed-object))
  
(define (stream-cons x y)
  (cons x (delay y)))
    
(define (stream-head stream) 
  (car stream))

(define (stream-tail stream) 
  (force (cdr stream)))

(define (stream-null? stream) 
  (eq? stream empty-stream))
  
(define (stream-ref stream n)
  (if (= n 0)
      (stream-head stream)
      (stream-ref (stream-tail stream) (- n 1))))

(define (stream-map procedure stream)
  (if (stream-null? stream)
      empty-stream
      (stream-cons
       (procedure (stream-head stream))
       (stream-map procedure (stream-tail stream)))))

(define (stream-for-each procedure stream)
  (if (stream-null? stream)
      empty-stream
      (begin 
        (procedure (stream-head stream))
        (stream-for-each procedure 
                         (stream-tail stream)))))

(define (display-line line)
  (display line)
  (newline))
  
(define (display-stream stream)
  (stream-for-each display-line stream))
  
(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))
  
(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         empty-stream)
        ((pred (stream-head stream))
         (stream-cons
          (stream-head stream)
          (stream-filter 
           pred
           (stream-tail stream))))
        (else (stream-filter 
               pred 
               (stream-tail stream)))))
               
(define (stream-join stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (stream-cons (stream-head stream1) 
                   (stream-join (stream-tail stream1) stream2))))

(define (list->stream l)
  (if (null? l)
    empty-stream
    (stream-cons (car l) (list->stream (cdr l)))))

(define (stream-varargs . args)
  (list->stream args))
  
(define (stream-print stream)
  (define (stream-print-elements stream)
    (if (not (null? stream))
        (let ((element (stream-head stream)))
          (display element)
          (display " ")
          (stream-print-elements (stream-tail stream)))))
  (display "{ ")
  (stream-print-elements stream)
  (display "}")
  (newline))
  
; testing  
(define my-stream (stream-cons 1 (stream-cons 2 (stream-cons 3 empty-stream))))
(stream-print my-stream)
(stream-print (stream-filter (lambda (x) (> x 5)) (stream-enumerate-interval 1 10)))
(display-stream my-stream)

(define my-stream-1 (stream-cons 1 (stream-cons 2 (stream-cons 3 empty-stream))))
(define my-stream-2 (stream-cons 9 (stream-cons 8 (stream-cons 7 empty-stream))))
(stream-print (stream-join my-stream-1 my-stream-2))

(stream-print (stream-varargs 5 3 6 3))

(stream-print (stream-filter (lambda (x) (zero? (mod x 2))) (stream-enumerate-interval 1000 2000)))
