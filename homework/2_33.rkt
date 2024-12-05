#lang sicp

(define (double x) (* 2 x))

(define (accumulate op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append seq1 seq2) (accumulate cons seq2 seq1))

(append (list 2 4) (list 4 6))

(define (length sequence) (accumulate (lambda (_ y) (+ 1 y) ) 0 sequence))
(length (list 1 3 7 66 6))

(define (map p sequence)
    (accumulate (lambda (x y)  (cons (p x) y)) nil sequence))
(map double (list 1 3 7))
