#lang sicp

(define (reverse list)
  (define (reverse-iter a list) 
    (if (null? (cdr list)) (cons (car list) a)
      (reverse-iter (cons (car list) a) (cdr list)) 
    )
  )
  (reverse-iter nil list)
)

(define (deep-reverse list)
  (define (reverse-iter a list) 
    (cond 
      ((null? (cdr list)) (cons (if (list? (car list)) (deep-reverse (car list)) (car list)) a))
      ((list? (car list)) (reverse-iter (cons (deep-reverse (car list)) a) (cdr list)))
      (else (reverse-iter (cons (car list) a) (cdr list)))))
  (reverse-iter nil list)
)
(deep-reverse (list 1 2))
(deep-reverse (list 1 4 9 16 25))
(define x (list (list 1 2) (list 3 4)))
(reverse x)


(deep-reverse x)