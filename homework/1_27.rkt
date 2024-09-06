#lang sicp
(define (square n) (* n n))

(define (expmod base exp m) (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
m)) (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (congruent a n)
  (= (expmod a n n) (modulo a n))
)

(define (test-cong int)
  (define (test-iter n)
    (cond ((= int n) #t)
          ((not (congruent n int)) #f)
          (else (test-iter (+ 1 n)))
    )
  )
  (test-iter 2)
)

(test-cong 561)
(test-cong 562)
(test-cong 563)
(test-cong 1105)
(test-cong 1729)
(test-cong 2465)
(test-cong 2821)
