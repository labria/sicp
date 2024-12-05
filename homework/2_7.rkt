#lang sicp

(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))


(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
(make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y) 
  (if (= 0 (lower-bound y)) (error "oops"))
  (if (= 0 (upper-bound y)) (error "da"))
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(let ((a (make-interval 1 3))
      (b (make-interval 5 7))
      (c (make-interval 0 7))
)
(display (add-interval a b))
(newline)
(display (sub-interval b a))
(newline)
(display (div-interval b a))
(newline)
(display (div-interval b c))
(newline)

)




(define (cons x y) (lambda (m) (m x y)))

(cons 1 3)
(define (pair) (lambda (m) (m 1 3)))


(define (first) (lambda (x y) x))
(define (rest) (lambda (x y) y))

(define (car z) (z first))
(define (cdr z) (z rest))