#lang sicp
(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (double n) (* 2 n))
(define (identity x) x)

(define (accumulate combiner null-value term a next b)
  (define (iter aa result)
    (if (> aa b) result 
        (iter (next aa) (combiner (term aa) result))))
  (iter a null-value))

(define (product term a next b) (accumulate * 1 term a next b))
(define (sum term a next b) (accumulate + 0 term a next b))

(define (fac n)
  (product identity 1 inc n))

(define (wallis n)
  (/ (square (double n)) (* (- (double n) 1) (+ (double n) 1)))
)
(fac 5)
(define (pi n) 
  (* 2 (product wallis 1 inc n))
)

(exact->inexact (pi 10))
(exact->inexact (pi 100))
(exact->inexact (pi 1000))
(exact->inexact (pi 10000))