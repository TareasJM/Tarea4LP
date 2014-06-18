#lang scheme
(define (addElement Lista1 Lista2) 
  (if (null? Lista1)
      (list Lista2) 
      (cons (car Lista1) (addElement (cdr Lista1) Lista2)
            )
      )
  )
(addElement (list'(a b c)) '(d e))