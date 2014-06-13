#lang scheme

(define puzzle (call-with-input-file "crossword_X.txt" read))

(define findInRow
  (lambda (n lista (s 0)(r '()))
    (cond
      ((empty? lista) (reverse r) )
      ((eq? n (car lista)) (begin (set! r (cons s r)) (findInRow n (cdr lista) (+ s 1) r )))
      (else (findInRow n (cdr lista) (+ s 1) r )))))

(findInRow '- (car puzzle))
(findInRow '- (car (cdr puzzle)))