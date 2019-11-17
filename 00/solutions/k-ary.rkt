#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
   (define (helper n k i)
    (if(= n 0)
       0
       (+ (* (remainder n 10) (expt k i)) (helper (quotient n 10) k (+ i 1))))
    )
  (helper n k 0)
  )

(define (to-k-ary n k)
  (define (helper n k i)
    (if (= n 0)
        0
        (+ (* (remainder n k) (expt 10 i)) (helper (quotient n k) k (+ i 1)))
        )
    )
  (helper n k 0)
  )



