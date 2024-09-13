#lang sicp
(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (double n) (* 2 n))
(define (identity x) x)
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))) (define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (gcd a b) (if (= b 0)
                      a
                      (gcd b (remainder a b))))

(define (faccumulate combiner null-value term filter a next b)
  (define (iter aa result)
    (cond ((> aa b) result)
          ((filter aa) (iter (next aa) (combiner (term aa) result)))
          (else (iter (next aa) result))))
  (iter a null-value))

(define (yes n) true)
(define (product term a next b) (faccumulate * 1 term yes a next b))
(define (sum term a next b) (faccumulate + 0 term a yes next b))

(define (square-primes a b)
  (faccumulate + 0 square prime? a inc b)
  )

(define (rps n)
  (define (relative-prime? a)
    (= 1 (gcd a n)))
  (faccumulate * 1 identity relative-prime? 0 inc n)
  )

(square-primes 2 10)
(rps 10)