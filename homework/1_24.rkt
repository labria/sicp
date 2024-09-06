#lang sicp
(define (square n) (* n n))
(define (next n) (if (= n 2) 3 (+ 2 n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) (define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m) (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
m)) (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n) 
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fermat-test n)
      (report-prime (- (runtime) start-time) n) #f))
(define (report-prime elapsed-time prime)
  (newline)
  (display prime)
  (display " *** ")
  (display elapsed-time)
  #t
  )

(define (first-prime start)
  (if (timed-prime-test start) start
      (first-prime (+ 2 start)))
  )
(define (first-n n start)
  (define (first-n-iter start acc)
    (if (= n (length acc)) acc
        (let ((fp (first-prime start)))
          (first-n-iter (+ fp 2) (append acc (list fp)))
          )
        )
    )
  (first-n-iter start '())
  )
(first-n 3 1000000001)
(first-n 3 10000000001)
(first-n 3 100000000001)
(first-n 3 1000000000001)