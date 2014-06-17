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
(define replaceCByRC
  (lambda (pzzl y x c (cy 0) (np '()))
    (cond
      ((empty? pzzl) (reverse np))
      ((eq? y cy) (begin (set! np (cons (replaceInList (car pzzl) x c) np)) (replaceCByRC (cdr pzzl) y x c (+ cy 1) np)))
      (else (begin (set! np (cons (car pzzl) np)) (replaceCByRC (cdr pzzl) y x c (+ cy 1) np)))
      )
    )
  )
; Intenta reemplazar una palabra en pzzl
(define replaceWByRCD
  (lambda (pzzl y x d w (np pzzl))
      (cond
        ((eq? d 'r) (begin (cond
                             ((empty? w) np)
                             ((empty? np) pzzl)
                             ((eq? '* (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'r (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'b (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? (car w) (getByRC np y x )) (begin (set! np (replaceCByRC np x y (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             (else pzzl)
                             ) ))
        )
      )
  )

(display  puzzle)
(newline)
;(display words)
;(newline)
(replaceWByRCD puzzle 2 0 'r (list 'a 'b 'c 'd 'e 'f 'g))
(display puzzle)