#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe l)
  (cond
      ((null? (cdr l)) (fringe (car l)))
      ((list? (car l)) (cons (fringe (car l)) (fringe (cdr l))))
      (else (cons (car l) (fringe (cdr l))))
  )
)
(fringe (list 1 2))
(fringe (list 1 2 3 5 5))
(fringe x)