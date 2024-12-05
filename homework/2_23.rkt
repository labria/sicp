#lang sicp

(define (square num) (* num num))

(define (for-each f l) (map f l))

(for-each (lambda (x) (newline)
            (display x))
          (list 57 321 88))
