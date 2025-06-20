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

(define (adjoin-set x sett)
  (cond ((null? sett) (list x))
        ((< (weight x) (weight (car sett))) (cons x sett))
        (else (cons (car sett) (adjoin-set x (cdr sett))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))



(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
  (else (element-of-set? x (cdr set)))))

(define (encode-symbol sym tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond 
            ((null? tree) '())    
            ((leaf? tree) '())    
            ((element-of-set? sym (symbols left)) (cons 0 (encode-symbol sym left)))
            ((element-of-set? sym (symbols right)) (cons 1 (encode-symbol sym right)))

      )

))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (insert-in-place leaf sorted-list)
  (if (null? sorted-list) (list leaf)
      (if (> (weight leaf) (weight (car sorted-list))) (cons (car sorted-list) (insert-in-place leaf (cdr sorted-list)))
          (cons leaf sorted-list))
      ))

(define sample-pair '((WAH 1) (BOOM 1) (A 2) (GET 2) (OB 2) (SHA 3) (YIP 9) (NA 16)))

(define sample-pair2 '((WAH 1) (BOOM 3)))
(make-leaf-set sample-pair2)

(make-leaf-set sample-pair)

(define (successive-merge l)
  (if (null? (cddr l)) (make-code-tree (car l) (cadr l))
      (successive-merge (insert-in-place (make-code-tree (car l) (cadr l)) (cddr l)))
  )
)
;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-tree (generate-huffman-tree sample-pair))

(define sample-message '(1 1))

(decode sample-message sample-tree]

sample-tree

(encode '('A) sample-tree)