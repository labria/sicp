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

(define (accumulate op initial sequence) (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (list-sum l) (accumulate + 0 l))
(define (filter predicate sequence) (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
(filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (remove item sequence)
(filter (lambda (x) (not (= x item)))
sequence))



(define (flatmap proc seq)
      (accumulate append nil (map proc seq)))


(define (permutations s)
      (if (null? s) ;emptyset?
          (list nil) ;sequencecontainingemptyset 
          (flatmap (lambda (x)
                    (map (lambda (p) (cons x p)) (permutations (remove x s)))) s )))


(permutations (list 2 4 7))

(define (square n) (* n n))

(define (enumerate-interval low high) (if (> low high)
nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))) (define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (all? fun . lists) (accumulate (lambda (a b) (and a b)) #t (apply map (cons fun lists))))

(define (queens board-size) 
  (define empty-board nil)
  (define (safe? k p) 
    (let ((offsets (enumerate-interval 1 (- k 1))))
      (and 
        (all? (lambda (pos) (not (= (car p) pos))) (cdr p))
        (all? (lambda (pos offset)  (not (= (car p) (+ offset pos)))) (cdr p) offsets)
        (all? (lambda (pos offset)  (not (= (car p) (- pos offset)))) (cdr p) offsets))))
  (define (adjoin-position new-row k rest-of-queens) (cons new-row rest-of-queens))
  (define (queen-cols k)
    (if 
      (= k 0)  
      (list empty-board)
      (filter (lambda (positions) (safe? k positions)) 
        (flatmap
          (lambda (rest-of-queens) 
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)) 
                 )
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(queens 4)
(length (queens 8))
