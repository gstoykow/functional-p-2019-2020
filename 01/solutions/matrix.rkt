#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
(define (all? p? xs)
  (or (null? xs)
      (and (p? (car xs))
           (all? p? (cdr xs)))))

; 01.
(define (any? p? xs)
  (and (not (null? xs))
       (or (p? (car xs))
           (any? p? (cdr xs)))))

; 02.
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))
      )
  )

(define (concat xss)
  (define (concat-help rs xss)
    (if (null? xss)
      rs
      (concat-help (my-append rs (car xss)) (cdr xss))
      )
    )
  (concat-help '() xss)
  )

; 03.
(define get-rows length)
(define (get-first-row m) (car m))
(define (del-first-row m) (cdr m))

(define (get-columns m) (length (car m)))
(define (get-first-column m) (map car m))
(define (del-first-column m) (map cdr m))

(define (rows xss)
  (if (null? xss)
      null
      (cons (car xss) (rows (cdr xss)))
      )
  )

; 04.
(define (cols xss)
  (if (null? (car xss))
      null
      (cons (get-first-column xss) (cols (del-first-column xss)))
      )
  )

; 05.
(define (matrix-ref xss i j)
  (if (eq? i 0)
      (list-ref (car xss) j)
      (matrix-ref (cdr xss) (- i 1) j)
      )
  )

; 06.
(define (set xs i x)
  (define (set-help xs i x ctr)
  (if (null? xs)
      '()
      (if (eq? ctr i)
          (cons x (set-help (cdr xs) i x (+ 1 ctr)))
          (cons (car xs) (set-help (cdr xs) i x (+ 1 ctr)))
          )
      )
  )
  (set-help xs i x 0)
  )

; 07.
(define (place xss i j x)
  (define (place-help xss i j x ctr)
    (if (null? xss)
        '()
        (if (eq? ctr i)
            (cons (set (car xss) j x) (place-help (cdr xss) i j x (+ 1 ctr)))
            (cons (car xss) (place-help (cdr xss) i j x (+ 1 ctr)))
            )
        )
    )
  (place-help xss i j x 0)
  )

; 08.
(define (diag matrix)
  (define (help-diag matrix ctr)
    (if (null? matrix)
        '()
        (cons (list-ref (car matrix) ctr) (help-diag (cdr matrix) (+ 1 ctr))
        )
    )
 )
  (help-diag matrix 0)
  )

; 09.
(define (reverse-cols matrix)
  (map reverse matrix)
  )

(define (diags xss)
  (cons (diag xss) (cons (diag (reverse-cols xss)) null))
  )


; 10.
(define (1+ x) (+ 1 x))
(define (id x) x)
(define (const x) (lambda (y) x))

(define (map-matrix f xss)
  (if (null? xss)
      '()
      (cons (map f (car xss)) (map-matrix f (cdr xss)))
      )
  )

; 11.
(define (filter-row p? xs)
  (if (null? xs)
      '()
      (if (p? (car xs))
          (cons (car xs) (filter-row p? (cdr xs)))
          (filter-row p? (cdr xs))
          )
      )
  )

(define (filter-matrix p? xss)
  (if (null? xss)
      '()
      (cons (filter-row p? (car xss)) (filter-matrix p? (cdr xss)))
      )
  )

; 12.
(define (zip-with fn xs ys)
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (fn (car xs) (car ys))(zip-with fn (cdr xs) (cdr ys)))
      )
  )

; 13.
(define (zip-matrix xss yss)
  (if (or (null? xss)
          (null? yss))
      '()
      (cons (zip-with cons (car xss) (car yss)) (zip-matrix (cdr xss) (cdr yss)))
      )
  )
