#lang sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
        (list entry left right))

(define (tree->list-1 tree)
(if (null? tree) '()
(append (tree->list-1 (left-branch tree))
(cons (entry tree)
(tree->list-1
(right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (t1)
  (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '())))
)

(define (t2)
  (list 3 (list 1 nil nil) (list 7 (list 5 nil nil) (list 9 nil (list 11 nil nil)) ))
)

(define (t3)
  (list 5 (list 3 (list 1 nil nil) nil) (list 9 (list 7 nil nil) (list 11 nil nil)))
)

(define (t4)
  (list 5 (list 3 (list 1 nil nil) nil) (list 9 (list 11 nil nil) (list 7 nil nil)))
)
(t1)
(t2)
(t3)
(tree->list-1 (t1))
(tree->list-2 (t1))

(tree->list-1 (t2))
(tree->list-2 (t2))

(tree->list-1 (t3))
(tree->list-2 (t3))

(tree->list-1 (t4))
(tree->list-2 (t4))