#lang sicp
; original recursive implementation
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 8)

; Iterative implementation with an accumulator
(define (exp b n)
  (define (iter-exp a b n)
    (cond ((= n 0) a)
          ((even? n) (iter-exp a (square b) (/ n 2)))
          (else (iter-exp (* a b) b (dec n)))))
  (iter-exp 1 b n)
  )

(exp 2 1)
(exp 2 2)
(exp 2 8)