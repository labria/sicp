#lang sicp

(define (f n)
  (if (< n 3) n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))
       )
      )
  )

(define (fff n)
  (define (f-iter c i1 i2 i3 n)
    (if (< n 3) n
        (if (= c (dec n))
            i1
            (f-iter (inc c)
                    (+ (* 1 i1)
                       (* 2 i2)
                       (* 3 i3)
                       )
                    i1 i2 n)
            ))
    )
  (f-iter 1 2 1 0 n)
  )


(fff 1)
(fff 2)
(fff 3)
(fff 4)
(fff 5)

(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
