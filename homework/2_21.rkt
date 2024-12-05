#lang sicp

(define (square num) (* num num))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (squares-list items)
  (map square items))



(square-list (list 1 2 3 4))
(squares-list (list 1 2 3 4))