#lang sicp

(define (double x) (* 2 x))

(define (accumulate op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op initial seqs) 
    (if (null? (car seqs))
        nil
        (cons (accumulate op initial (map car seqs)) 
              (accumulate-n op initial (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
   (map (lambda (x) (dot-product v x)) m))


(define (transpose mat) 
    (accumulate-n cons nil mat))


(define (matrix-*-matrix m n) 
    (let ((cols (transpose n)))
          (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define (matrix-*-matrix2 m n) 
    (let ((cols (transpose n)))
          (map (lambda (row) (matrix-*-vector cols row)) m)))


(define mat '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define mat2 '((0 1) (2 3) (3 5) (0 0)))
mat
(transpose mat2)
(transpose mat)
;mat
(define vec '(0 1 0 4))
(matrix-*-vector mat vec)
(matrix-*-matrix mat mat2)
(matrix-*-matrix2 mat mat2)