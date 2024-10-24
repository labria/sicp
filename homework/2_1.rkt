#lang sicp
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (average x y) (/ (+ x y) 2))


(define (square n) (* n n))

(define (make-rat n d)
  (let ((g (gcd n d))
        (c (if (positive? d) 1 -1)))
    (cons (* c (/ n g)) (* c (/ d g))))
  )

(make-rat -2 3)
(make-rat -2 -3)
(make-rat 2 -3)
(make-rat 2 3)