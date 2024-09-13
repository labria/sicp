#lang sicp
(define (cube x) (* x x x))
(define (square num) (* num num))

(define (inc n) (+ n 1))
(define (even? n)
  (= (remainder n 2) 0))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (*
   (sum f (+ a (/ dx 2.0)) add-dx b)
   dx)
  )

(define (i f a b n)
  (define (fk k)
    (f (+ a (* k (exact->inexact (/ (- b a) n)))))
  )
  (define (fr k)
    (cond ((= k 0) (fk k))
          ((= k n)  (fk k))
          ((even? k) (* 2 (fk k)))
          (else (* 4 (fk k)))))
  (* (sum fr 0 inc n) (/ (/ (- b a) n) 3))
)


(integral cube 0 1 0.01)

(i cube 0 1 20)

(i square 0 1 1000000000)
(i square 0 1 10)