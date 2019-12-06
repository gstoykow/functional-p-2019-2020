#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!

(define (get-columns m) (length (car m)))
(define (get-first-column m) (map car m))
(define (del-first-column m) (map cdr m))

(define (X? c)
  (eq? c "X")
  )

(define (O? c)
  (eq? c "O")
  )

(define (XO? c)(or (eq? c "X") (eq? c "O")))

(define (win-by-row p? b)
  (if (null? b)
      #f
      (or (all? p? (car b))
      (win-by-row p? (cdr b))
      )
  )
  )

(define (transpose m)
  (if (null? (car m)) '()
      (cons (get-first-column m) (transpose (del-first-column m)))))

(define (win-by-col p? b)
  (win-by-row p? (transpose b))
  )

(define (win-by-mdiag p? b)
  (all? p? (car (diags b)))
  )

(define (win-by-sdiag p? b)
  (all? p? (cadr (diags b)))
  )

(define (Df? xss)
           (if (null? xss)
               #t
               (and (all? XO? (car xss))
                   (Df? (cdr xss))
                   )
               )
           )

(define (winner b)
  (cond ((win-by-col X? b) "X" )
        ((win-by-col O? b) "O" )
        ((win-by-row X? b) "X" )
        ((win-by-row O? b) "O" )
        ((win-by-mdiag X? b) "X" )
        ((win-by-mdiag O? b) "O" )
        ((win-by-sdiag X? b) "X" )
        ((win-by-sdiag O? b) "O" )
        (else
        (if(Df? b) "D"
           #f
           )
         )
  )
  )

; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!
(define (play curr-board curr-sign)
  (define (helper i j)
    (cond ((> i 2) #f)
          ((> j 2) (helper (+ i 1) 0))
          ((not (list-ref (list-ref curr-board i) j)) (cons i j))
          (else (helper i (+ j 1)))))
  (helper 0 0))
