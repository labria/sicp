#lang sicp

; Original implementation
(define (square num) (* num num))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x) (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

; Exercise 1.7, delta-based implementation
(define (close-enough? previous last)
  (< (abs (/ (- last previous) previous)) 0.001))

(define (sqrt-iter-delta previous-guess guess x)
  (if (close-enough? previous-guess guess)
    guess
    (sqrt-iter-delta guess (improve guess x) x)))

(define (sqrt-delta x) (sqrt-iter-delta 1000 1.0 x))

; runs
(sqrt 0.01)
(sqrt-delta 0.01)
; ^ better for small
(sqrt 1000000)
(sqrt-delta 1000000)
; ^ but worse for big