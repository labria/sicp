#lang sicp

(define (double x) (* 2 x))

(define (accumulate op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leavess x) (cond ((null? x) 0)
                                ((not (pair? x)) 1)
                                (else (+ (count-leavess (car x))
                                         (count-leavess (cdr x))))))


(define (count-leaves t)
    (accumulate (lambda (x y) (if (pair? x) y (+ 1 y))) 0 (map fringe t)))

(let ( (list (tree (list 1 (list 23 4)))))
    (count-leaves tree))



