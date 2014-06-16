#lang scheme
;Lee puzzle.txt y lo pasa a lista
(define puzzle (call-with-input-file "crossword_X.txt" read))

;Lee words.txt y lo pasa a lista
(define getWords
  (lambda ((list '()) (file (call-with-input-file "words_X.txt" read)))
    (cond
      ((empty? file) list)
      (else (getWords (cons (string->list (car file)) list) (cdr file)))
      )
    )
  )
(define words (getWords))
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
    (getInList (getInList puzzle y) x)
    )
  )

(getByRC puzzle 0 10)