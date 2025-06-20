#lang sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
  ((equal? x (car set)) true)
  (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))



(element-of-set? 3 (list 1 3 3 5))
(adjoin-set 4 (list 1 3 5))

(union-set (list 1 2 2 5) (list 1 3 4 5))
(intersection-set (list 1 2 2 5) (list 1 3 4 5))
(union-set (list 1) (list 3 5))