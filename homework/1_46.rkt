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

(define (iterative-improve good? improve)
  (lambda (x)
    (define (iter g1 g2)
      (if (good? g1 g2 ) g1
          (iter (improve g1 g2) g2)
          )
      )
    (iter 2.0 x)
    )
  )


(define (sqrtt n)
  (define (improver guess x) (average guess (/ x guess)))
  (define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improver) n)
  )

(sqrtt 65)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (improver v1 v2) (display v1) (newline) (display v2) (f v1))
  ((iterative-improve close-enough? improver) first-guess)
  )

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 65)