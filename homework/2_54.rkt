#lang sicp


(define (equall? l1 l2)
  (cond ((eq? l1 l2) #t)
        (else (let ((l1h (car l1))
        (l1t (cdr l1))
        (l2h (car l2))
        (l2t (cdr l2)))
        (and (symbol? l1h) (symbol? l2h) (eq? l1h l2h) (equall? l1t l2t))
      ))
  )
)

(equall? '(this is a list) '(this is a list))
(equall? '(this is a list) '(this (is a) list))
(equall? '(this 1 a list) '(this 1 a list))
(equall? 'this 'that)