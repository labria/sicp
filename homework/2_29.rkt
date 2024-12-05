#lang sicp


(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (simple-weight? branch) (number? (branch-structure branch)))

(define (torque branch) (* (branch-weight branch) (branch-length branch)))

(define (branch-weight branch)
  (if (simple-weight?  branch) (branch-structure branch) (total-weight (branch-structure branch)) )
)

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))
)

(define (balanced-branch? branch)
  (if (simple-weight? branch) #t (balanced? (branch-structure branch)))
)

(define (balanced? mobile)
  (and (= (torque (right-branch mobile)) (torque (left-branch mobile)))
       (balanced-branch? (right-branch mobile))
       (balanced-branch? (left-branch mobile))
  )
)

(let* ((llb (make-branch 10 11))
       (lrb (make-branch 5 22))
       (lbs (make-mobile llb lrb))
       (lb  (make-branch 4 lbs))
       (rb  (make-branch 2 66))
       (mobile (make-mobile lb rb)))
  (balanced? mobile)
)
