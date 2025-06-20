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


(list->tree (list 1 3 5 7 9 11))
(list->tree (list 1 2 3 5 7 9 11))

(list->tree (list 1 3 5))
(list->tree (list 2 5 6 7))