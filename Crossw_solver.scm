#lang scheme
;Lee puzzle.txt y lo pasa a lista
(define puzzle (call-with-input-file "crossword_X.txt" read))

;Lee words.txt y lo pasa a lista
(define words
 (letrec
  ((gw
   (lambda ((list '()) (file (call-with-input-file "words_X.txt" read)))
    (cond
      ((empty? file) list)
      (else (gw (cons (string->list (car file)) list) (cdr file)))
      )
    )
  ))
  (gw)
 )
)

;Encuentra elemento de la lista en la posicion X 
(define getInList
  (lambda (list x (cx 0))
    (cond
      ((empty? list) #f)
      ((eq? list #f) #f)
      ((= cx x)  (car list))
      (else (getInList (cdr list) x (+ cx 1)))
     )
    )
  )
;Encuentra elemento en pzzl en fila row y columna col
(define getByRC
  (lambda (pzzl y x)
    (getInList (getInList pzzl y) x)
    )
  )
(display puzzle)
(newline)
(display words)
(newline)
(getByRC puzzle 0 10)
(getByRC words 1 3)
;Reeplaza un elemto en list con posicion x
(define replaceInList
  (lambda (list x c (cx 0) (nl '()))
    (cond
      ((empty? list) (reverse nl))
      ((eq? x cx) (begin (set! nl (cons c nl)) (replaceInList (cdr list) x c (+ cx 1) nl)))
      (else (begin (set! nl(cons (car list) nl)) (replaceInList (cdr list) x c (+ cx 1) nl)))
      )
    )
  )
;Reeplaza un elemento en pzzl en la pos row,col
(define replaceByRC
  (lambda (pzzl y x c (cy 0) (np '()))
    (cond
      ((empty? pzzl) (reverse np))
      ((eq? y cy) (begin (set! np (cons (replaceInList (car pzzl) x c) np)) (replaceByRC (cdr pzzl) y x c (+ cy 1) np)))
      (else (begin (set! np (cons (car pzzl) np)) (replaceByRC (cdr pzzl) y x c (+ cy 1) np)))
      )
    )
  )

;(replaceInList (list 1 2 3 4 5) 0 'X )
(replaceByRC puzzle 5 0 'X)