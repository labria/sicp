#lang sicp
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (average x y) (/ (+ x y) 2))


(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= 1 n)
      (lambda (x) (f x))
      (compose f (repeated f (dec n)))
      )
  )

(define (smooth f)
  (let ((dx 0.1))
    (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))
  )


((smooth square) 10)

(((repeated smooth 10) square) 10)


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))




(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 100)

(define (qroot x)
  (fixed-point (lambda (y) (average y (/ x (square y))))
               1.0))
(qroot 1000)


(define (average-damp f)
(lambda (x) (average x (f x))))


(define (root n k x)
  (display "root ")
  (display n)
  (newline)
  (fixed-point ((repeated average-damp k) (lambda (y) (average y (/ x (fast-expt y (dec n))))))
               1.0))

(define (sroot n x)
  (display "sroot ")
  (display n)
  (newline)
  (let ((k (inexact->exact (ceiling (log n)))))
  (fixed-point ((repeated average-damp k) (lambda (y) (average y (/ x (fast-expt y (dec n))))))
               1.0)))

(root 2 1 1000)
(root 3 1 1000)
(root 4 2 1000)
(root 5 2 1000)
(root 6 2 1000)
(root 7 2 1000)
(root 8 2 1000)
(root 9 2 1000)
(root 10 2 1000)
(root 100 3 1000)
(root 1000 6 1000000)
(root 10000 8 1000000)
(root 100000 12 10000000000)
(sroot 100000 10000000000)
(sroot 10000000 10000000000)
\