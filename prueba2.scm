#lang scheme
(define (addElement Lista1 Lista2) 
  (if (null? Lista1)
      (list Lista2) 
      (cons (car Lista1) (addElement (cdr Lista1) Lista2)
            )
      )
  )


(define funcion
  (lambda (lista)
    (let funcion1 ((LG lista) (LR (list'())) (x 0) (y 0) (aux 8) (largo 8))
      (cond
        (
         ((equal? aux 0)
         (begin
           (set! y (+ y 1))
           (set! aux (= aux 8))
           (funcion1 (car(cdr LG)) LR x y aux largo)
           ))
         ((and((equal? (car(car LG)) #\r) (equal? (car(car(cdr LG))) #\*)))
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\r))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         ((and((not (equal? (car(car LG)) #\r)) (not(equal? (car(car(cdr LG))) #\*))))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))                                               
         ((and((equal? (car(car LG)) #\d) (equal? (car(car(cdr LG))) #\*)))
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\d))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         ((and((not (equal? (car(car LG)) #\d)) (not(equal? (car(car(cdr LG))) #\*))))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            )) 
         ((and((equal? (car(car LG)) #\b) (equal? (car(car(cdr LG))) #\*)))
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\b))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         ((and((not (equal? (car(car LG)) #\b)) (not(equal? (car(car(cdr LG))) #\*))))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))             
         ((equal? y (length LG))LR)
          )
        )
      )
    )
  )
           
	 