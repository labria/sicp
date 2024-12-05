#lang sicp

(define (filter-by f l)
  (define (filter-iter a l) 
   (cond ((null? l) a)
         ((f (car l)) (filter-iter (append a (list (car l))) (cdr l)))
         (else (filter-iter a (cdr l)))
  ))
  (filter-iter '() l)
)

(define (same-parity x . y)
  (if (odd? x) (filter-by odd? y) (filter-by even? y))
)

(define (sames-parity x . y)
  (filter-by (if (odd? x) odd? even?) y)
)


(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(sames-parity 1 2 3 4 5 6 7)
(sames-parity 2 3 4 5 6 7)