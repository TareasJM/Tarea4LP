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

;Lee todas los inicios de palabra en pzzl
(define positions (list '(r 0 9) '(r 1 3) '(r 2 0) '(r 4 5) '(d 0 9) '(d 1 3) ))
;(define positions (list '(d 1 3) '(r 2 0)))

;Auxiliar para permute
(define (insert l n e)
  (if (= 0 n)
      (cons e l)
      (cons (car l) 
            (insert (cdr l) (- n 1) e))))

;Auxiliar para permute
(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))

;Todas las posibles permutaciones de la lista l 
(define (permute l)
  (if (null? l)
      '(())
      (apply append (map (lambda (p)
                           (map (lambda (n)
                                  (insert p n (car l)))
                                (seq 0 (length p))))
                         (permute (cdr l))))))

(set! positions (permute positions))

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
                             ((empty? w) (if (not(or (eq? (getByRC np y x) '-)(eq? (getByRC np y x) #f))) #f np))
                             ((empty? np) #f)
                             ((eq? '* (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'r (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'b (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? (car w) (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             (else #f)
                             ) ))
        ((eq? d 'd) (begin (cond
                             ((empty? w) (if (not(or (eq? (getByRC np y x) '-)(eq? (getByRC np y x) #f))) #f np))
                             ((empty? np) #f)
                             ((eq? '* (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? 'd (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? 'b (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? (car w) (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             (else #f)
                             ) ))
        )
      )
  )

(define solveCrssw2
  (lambda (pzzl wrds pstns pl (np '()))
    (if (empty? wrds) pzzl
      (begin
        (display pstns)(newline)(display pl)(newline)
        (set! np (replaceWByRCD pzzl (getByRC pl 0 1)(getByRC pl 0 2)(getByRC pl 0 0) (car wrds)))
        (if (eq? np #f)
          (solveCrssw2 pzzl wrds pstns (cdr pl)) (solveCrssw2 np (cdr wrds) pstns pstns))
        )
      )
    )
  )
(define solveCrssw
  (lambda (pzzl wrds pstns cw cp np)
    (if (or (empty? cw) (empty? pstns)) np
        (begin
          ;(display cw)(newline)
          (set! np (replaceWByRCD np (getByRC cp 0 1)(getByRC cp 0 2)(getByRC cp 0 0) (car cw)))
          (if (eq? np #f)
              (solveCrssw pzzl wrds (cdr pstns) wrds (car pstns) pzzl) (solveCrssw pzzl wrds pstns (cdr cw) (cdr cp) np))
          )
      )
    )
  )
;(replaceWByRCD puzzle 6 11 'd (list '-))
;(display positions)
(define solution (solveCrssw puzzle words (cdr positions) words (car positions) puzzle))

(call-with-output-file "solution.txt"
  (lambda (output-port)
    (begin
      (display solution output-port)
      )
    )
  )

;(getByRC words 0 1)