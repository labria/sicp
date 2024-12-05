#lang sicp

(define (reverse list)
  (define (reverse-iter a list) 
    (if (null? (cdr list)) (cons (car list) a)
      (reverse-iter (cons (car list) a) (cdr list)) 
    )
  )
  (reverse-iter nil list)
)
(reverse (list 1 4 9 16 25))