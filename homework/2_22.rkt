#lang sicp

(define (square num) (* num num))


(define (square-list items) 
      (define (iter things answer)
(if (null? things) answer
        (iter (cdr things)
              (cons (square (car things))
answer))))
(iter items nil))
(define (squares-list items)
(define (iter things answer) (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
(squares-list (list 1 2 3 4))