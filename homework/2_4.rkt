#lang sicp
(define (cons x y) (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (make-point x y)
  (cons x y)
  )
(define (x-point p)
  (car p)
  )

(define (y-point p)
  (cdr p)
  )

(define (make-rect a b)
  (cons a b)
  )
(define (a-corner p)
  (car p)
  )

(define (b-corner p)
  (cdr p)
  )

(define (width rect)
  (abs (- (x-point (a-corner rect)) (x-point (b-corner rect))))
  )

(define (height rect)
  (abs (- (y-point (a-corner rect)) (y-point (b-corner rect))))
  )

(define (perimeter rect)
  (* 2 (+ (width rect) (height rect)))
  )

(define (area rect)
  (* (width rect) (height rect))
  )

(let* ((p1 (make-point 1 1))
       (p2 (make-point 5 6))
       (rect (make-rect p1 p2)))
  (display (perimeter rect))
  (newline)
  (display (area rect))
  )