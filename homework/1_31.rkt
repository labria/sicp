#lang sicp
(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (double n) (* 2 n))
(define (product term a next b)
  (define (iter aa result)
    (if (> aa b) result
        (iter (next aa) (* (term aa) result))))
  (iter a 1))

(define (identity x) x)

(define (fac n)
  (product identity 1 inc n))

(define (wallis n)
  (/ (square (double n)) (* (- (double n) 1) (+ (double n) 1)))
)
(fac 5)
(define (pi n) 
  (* 2 (product wallis 1 inc n))
)

(exact->inexact (pi 5))
(exact->inexact (pi 100))