#lang scheme
(define numbers '(1 2 3))

(define suma
  (lambda ((s 0) (n numbers) (a '()) (c '()))
    (cond
      ((empty? n) a)
      ((= s 4)(begin (set! a (cons c a))) a )
      ((> s 4) a )
      (else )
      )
    )
  )
(define findInRow
  (lambda (n lista (s 0)(r '()))
    (cond
      ((empty? lista) (reverse r) )
      ((eq? n (car lista)) (begin (set! r (cons s r)) (findInRow n (cdr lista) (+ s 1) r )))
      (else (findInRow n (cdr lista) (+ s 1) r )))))
