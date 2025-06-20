#lang sicp
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-decoded '(A D A B B C A))

(decode sample-message sample-tree)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
  (else (element-of-set? x (cdr set)))))

(define (encode-symbol sym tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond 
            ((leaf? tree) '())    
            ((element-of-set? sym (symbols left)) (cons 0 (encode-symbol sym left)))
            ((element-of-set? sym (symbols right)) (cons 1 (encode-symbol sym right)))

      )

))

(encode-symbol 'A sample-tree)
(encode-symbol 'B sample-tree)
(encode-symbol 'C sample-tree)
(encode-symbol 'D sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode sample-decoded sample-tree)


; (define (insert-in-place leaf sorted-list)
;   (if (null? sorted-list) (list leaf)

;   (if (> (weight-leaf leaf) (weight-leaf (car sorted-list))) (cons (car sorted-list) (insert-in-place leaf (cdr sorted-list)))
;                                                       (cons leaf sorted-list)
    
;   )
; ))

; (define sample-sorted (list (make-leaf 'A 1) (make-leaf 'B 3) (make-leaf 'C 5)))

; (insert-in-place (make-leaf 'D 0) sample-sorted)
; (insert-in-place (make-leaf 'D 2) sample-sorted)
; (insert-in-place (make-leaf 'D 7) sample-sorted)