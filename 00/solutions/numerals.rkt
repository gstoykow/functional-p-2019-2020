#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n)
   (define (1+ n) (+ 1 n))
   (n 1+ 0)
  )

(define (to-numeral n)
  (lambda (f x)
    (if (= n 0)
        x
        (f ((to-numeral (- n 1))f x) )
        )
    )
  )

(define (plus n m) void)

(define (mult n m) void)

(define (pred n) void)
