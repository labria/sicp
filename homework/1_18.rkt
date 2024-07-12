#lang sicp
; original recursive implementation
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))
(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))

(fast-* 1 3)
(fast-* 3 8)
(fast-* 2 8)
(fast-* 5 1)
(fast-* 2 2)

(define (iter-* a b)
  (define (inner acc a b)
    (cond ((= b 0) acc)
        ((even? b) (inner acc (double a) (halve b)))
        (else (inner (+ acc a) a (dec b)))))
  (inner 0 a b)
  )
(display "iter\n")
(iter-* 1 2)
(iter-* 1 3)
(iter-* 1 7)
(iter-* 3 1)
(iter-* 2 2)
(iter-* 3 8)
(iter-* 2 8)
(iter-* 8 2)
(iter-* 8 0)