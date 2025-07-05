#lang sicp
(define (square x) (* x x))

(define (make-from-real-imag x y)
(define (dispatch op)
(cond ((eq? op 'real-part) x)
((eq? op 'imag-part) y)
((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
((eq? op 'angle) (atan y x))
(else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
dispatch)


(define (make-from-real-imag x y)
(make-from-real-imag-rectangular x y))

(define (make-from-real-imag-rectangular x y)
(attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
(attach-tag 'rectangular
(cons (* r (cos a)) (* r (sin a)))))



(define (make-from-mag-ang r a)
(make-from-mag-ang-polar r a))
(define (make-from-mag-ang-polar r a)
(attach-tag 'polar (cons r a)))


(define (make-from-mag-ang-polar r a)
(attach-tag 'polar (cons r a)))
