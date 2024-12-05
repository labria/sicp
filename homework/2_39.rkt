#lang sicp

(define (double x) (* 2 x))

(define (fold-right op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence) (define (iter result rest)
(if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
(iter initial sequence))



(define (reverse sequence)
(fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reversel sequence)
(fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse '(1 3 5))
(reversel '(1 6 9))