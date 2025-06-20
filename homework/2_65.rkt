#lang sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
        (list entry left right))



(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))





(define (list->tree elements)
(car (partial-tree elements (length elements))))
(define (partial-tree elts n)
(if (= n 0)
(cons '() elts)
(let ((left-size (quotient (- n 1) 2)))
(let ((left-result
(partial-tree elts left-size)))
(let ((left-tree (car left-result))
(non-left-elts (cdr left-result))
(right-size (- n (+ left-size 1))))
(let ((this-entry (car non-left-elts))
(right-result
(partial-tree
(cdr non-left-elts)
right-size)))
(let ((right-tree (car right-result))
(remaining-elts
(cdr right-result)))
(cons (make-tree this-entry
left-tree
right-tree)
remaining-elts))))))))



(list->tree (list 1 2 5))
(list->tree (list 1 3 4 5))
(list->tree (list 1 2 3 4 5))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (entry set1) (entry set2)) (make-tree (entry set1)
                                                  (union-set (left-branch set1) (left-branch set2))
                                                  (union-set (right-branch set1) (right-branch set2))))
        ((< (entry set1) (entry set2)) (make-tree (entry set1)
                                                  (union-set (left-branch set1) (left-branch set2)) 
                                                  (union-set (right-branch set1) set2)))
        ((> (entry set1) (entry set2)) (make-tree (entry set1)
                                                  (union-set (left-branch set1) set2)
                                                  (right-branch set1)))        
                                        
        (else (display "opps"))))
(union-set (list->tree (list 1 2 5)) (list->tree (list 1 3 4 5)))

(union-set (list 1) (list 3 5))