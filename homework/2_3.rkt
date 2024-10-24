#lang sicp
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (average x y) (/ (+ x y) 2))


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