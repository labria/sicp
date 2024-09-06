#lang sicp
(define (square n) (* n n))

(define (ntr a n)
  (= 1 (modulo (square a) n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (ntr base exp) 0
             (remainder
              (square (expmod base (/ exp 2) m)) m)) )
        (else
         (remainder(* base (expmod base (- exp 1) m))m))))

(define (mr-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(mr-test 561)
(mr-test 562)
(mr-test 563)
(mr-test 1105)
(mr-test 1729)
(mr-test 2465)
(mr-test 2821)
