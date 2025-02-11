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

(define (make-segment x y)
  (cons x y)
)
(define (start-segment p)
  (car p)
)

(define (end-segment p)
  (cdr p)
)

(define (midpoint-segment-1 segment)
  (let ((s (start-segment segment))
       (e (end-segment segment)))
       (let ((x (average (x-point s) (x-point e)))
       (y (average (y-point s) (y-point e))))
       (make-point x y))
       ))

(define (midpoint-segment segment)
  (let* ((s (start-segment segment))
         (e (end-segment segment))
         (x (average (x-point s) (x-point e)))
         (y (average (y-point s) (y-point e))))
        (make-point x y))) 

(define (print-point p) 
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(let ((p1 (make-point 1 1)) 
(p2 (make-point 3 3)) )
(print-point (midpoint-segment (make-segment p1 p2))))
