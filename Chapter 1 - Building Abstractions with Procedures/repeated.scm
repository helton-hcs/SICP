#lang scheme

(define (repeat-with-args func times args)
  (lambda ()    
    (cond ((> times 0)           
           ((repeat-with-args func (- times 1) args))
           (func args)))))

(define (repeat func times)
  (repeat-with-args (lambda(args) (func)) times null))


(define call-hello-n-times
  (repeat (lambda()
              (display "hello") 
              (newline))
          5))

(define call-hello-n-times-with-args
  (repeat-with-args (lambda(args)
                        (display "hello") 
                        (display args)
                        (newline))
                      5
                      '(1 2 3)))

(call-hello-n-times)
(call-hello-n-times-with-args)