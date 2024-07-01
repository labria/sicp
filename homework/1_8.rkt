#lang sicp


; Exercise 1.8, delta-based implementation of cube-root
(define (close-enough? previous last)
  (< (abs (/ (- last previous) previous)) 0.001))

(define (croot-iter previous-guess guess x)
  (if (close-enough? previous-guess guess)
    guess
    (croot-iter guess (improve guess x) x)))

(define (improve guess x)
  (/ (+ (* 2 guess) (/ x (* guess guess))) 3))

(define (croot x) (croot-iter x 1.0 x))

; runs
(croot 1000000)
(croot 1)