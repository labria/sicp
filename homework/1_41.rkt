#lang sicp
(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (double f) 
  (lambda (x) (f (f x))))


((double inc) 4)

(((double (double double)) inc) 5)