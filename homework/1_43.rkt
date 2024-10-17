#lang sicp
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (square n) (* n n))
(define (compose f g) 
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= 1 n)
    (lambda (x) (f x))
    (compose f (repeated f (dec n)))
  )
)

((repeated square 2) 5)