#lang sicp
; original recursive implementation
(define (even? n)
  (= (remainder n 2) 0))
(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))

(fast-* 1 2)
(fast-* 1 3)
(fast-* 4 1)
(fast-* 2 8)
(fast-* 6 3)