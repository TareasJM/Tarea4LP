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
    (let funcion1 ((LG lista) (LR (list '())) (x 0) (y 0) (aux (length(car lista))) (largo (length(car lista))))
      (cond
        (
         ((null? LG)LR)
         
         ((equal? aux 0)
          
         (begin
           (set! y (+ y 1))
           (set! aux (= aux length(car(lista))))
           (funcion1 (car(cdr LG)) LR x y aux largo)
           ))
         
         ((equal? (car(car LG)) #\r)
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\r))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         
         ((equal? (car(car LG)) #\d)
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\d))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         
         ((equal? (car(car LG)) #\b)
          (begin
            (set! x (- largo aux))
            (set! aux(- aux 1))
            (addElement (LR #\b))
            (addElement (LR x))
            (addElement (LR y))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))
         
         ((not (equal? (car(car LG)) #\r))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))                                               
         
         ((not (equal? (car(car LG)) #\d))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            )) 
         
         ((not (equal? (car(car LG)) #\b))
          (begin
            (set! aux(- aux 1))
            (funcion1 (car(cdr LG)) LR x y aux largo)
            ))             
          )
        )
      )
    )
  )


           
	 