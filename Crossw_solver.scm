#lang scheme
;Numero del puzzle a solucionar
(define pzzlnum "X")
(define crsswin (string-append  "crossword_" (string-append pzzlnum ".txt")))
(define crsswout (string-append  "solve_" (string-append pzzlnum ".txt")))
(define wrdsin (string-append  "words_" (string-append pzzlnum ".txt")))

;***** funcion anonima ****
;Lee puzzle.txt y lo pasa a lista
;**************************
(define puzzle (car (cdr (call-with-input-file crsswin read))))

;***** funcion anonima ****
;Lee words.txt y lo pasa a lista
;**************************
(define words
 (letrec
  ((gw
   (lambda ((list '()) (file (car (cdr (call-with-input-file wrdsin read)))))
    (cond
      ((empty? file) list)
      (else (gw (cons (string->list (car file)) list) (cdr file)))
      )
    )
  ))
  (gw)
 )
)

;***** sendBack *****
;Envia el primer elemento al final de la lista
;**************************
(define sendBack
  (lambda (lista) (append (cdr lista) (list (car lista))))
  )
;***** inList? *****
;Retorna si un elemnto se encuentra en una lista
;**************************
(define inList?
  (lambda (lista x)
    (cond
      ((empty? lista) #f)
      ((eq? (car lista) x) #t)
      (else (inList? (cdr lista) x))
      )
    )
  )

;***** getInList *****
;Encuentra elemento de la lista en la posicion X 
;**************************
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

;***** getByRC *****
;Encuentra elemento en pzzl en fila row y columna col
;**************************
(define getByRC
  (lambda (pzzl y x)
    (getInList (getInList pzzl y) x)
    )
  )

;***** replaceInList *****
;Reeplaza un elemto en list con posicion x
;**************************
(define replaceInList
  (lambda (list x c (cx 0) (nl '()))
    (cond
      ((empty? list) (reverse nl))
      ((eq? x cx) (begin (set! nl (cons c nl)) (replaceInList (cdr list) x c (+ cx 1) nl)))
      (else (begin (set! nl(cons (car list) nl)) (replaceInList (cdr list) x c (+ cx 1) nl)))
      )
    )
  )

;***** replaceCByRC *****
;Reeplaza un elemento en pzzl en la pos row,col
;**************************
(define replaceCByRC
  (lambda (pzzl y x c (cy 0) (np '()))
    (cond
      ((empty? pzzl) (reverse np))
      ((eq? y cy) (begin (set! np (cons (replaceInList (car pzzl) x c) np)) (replaceCByRC (cdr pzzl) y x c (+ cy 1) np)))
      (else (begin (set! np (cons (car pzzl) np)) (replaceCByRC (cdr pzzl) y x c (+ cy 1) np)))
      )
    )
  )

;***** replaceWByRCD *****
; Intenta reemplazar una palabra en pzzl
;**************************
(define replaceWByRCD
  (lambda (pzzl y x d w (np pzzl))
      (cond
        ((eq? d 'r) (begin (cond
                             ((empty? w) (if (not(or (eq? (getByRC np y x) '-)(eq? (getByRC np y x) #f))) #f np))
                             ((empty? np) #f)
                             ((eq? '* (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'r (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'd (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? 'b (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             ((eq? (car w) (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np y (+ x 1) d (cdr w))))
                             (else #f)
                             ) ))
        ((eq? d 'd) (begin (cond
                             ((empty? w) (if (not(or (eq? (getByRC np y x) '-)(eq? (getByRC np y x) #f))) #f np))
                             ((empty? np) #f)
                             ((eq? '* (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? 'r (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? 'd (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? 'b (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             ((eq? (car w) (getByRC np y x )) (begin (set! np (replaceCByRC np y x (car w) )) (replaceWByRCD np (+ y 1) x d (cdr w))))
                             (else #f)
                             ) ))
        )
      )
  )

;***** getPositions *****
;Lee y retorna todos los inicios de palabra en pzzl
;**************************
(define getPositions
  (lambda (pzzl (cr 0)(cc 0)(pos '()))
    (cond
      ((empty? pzzl) pos )
      ((eq? (getInList (car pzzl) cc) #f) (getPositions (cdr pzzl) (+ cr 1) 0 pos ) )
      ((eq? (getInList (car pzzl) cc) 'r) (begin (set! pos (cons (list 'r cr cc) pos)) (getPositions pzzl cr (+ cc 1) pos )) )
      ((eq? (getInList (car pzzl) cc) 'd) (begin (set! pos (cons (list 'd cr cc) pos)) (getPositions pzzl cr (+ cc 1) pos )) )
      ((eq? (getInList (car pzzl) cc) 'b) (begin (set! pos (cons (list 'r cr cc) pos)) (set! pos (cons (list 'd cr cc) pos)) (getPositions pzzl cr (+ cc 1) pos )) )
      (else (getPositions pzzl cr (+ cc 1) pos ))
      )
    )
  )
(define positions (getPositions puzzle))

;***** writeSol *****
;escribe la solucion en el archivo solve_X.txt correspondiente
;**************************
(define writeSol
  (lambda (sol (file (open-output-file crsswout)) (b #t))
    (cond
        ((empty? sol)(begin (display ")" file) (close-output-port file) "Solucion guardada"))
        ((eq? b #t) (begin (display "'(" file) (writeSol sol file #f)))
        (else
          (begin
            (display (car sol) file)(display (car sol))
            (newline file)(newline)
            (writeSol (cdr sol) file #f)
            )
          )
        )
    )
  )

(define show
  (lambda (pzzl)
    (if (empty? pzzl)(begin (display "------------------------------------------------------------------------------------------------")(newline))(begin (display (car pzzl))(newline)(show (cdr pzzl))))
    )
  )

;***** solveCrossw *****
;Funcion principal que usa todas las anteriores para encontrar unna solucion
;**************************
(define solveCrssw
  (lambda (pzzl wrds pstns (oldPzzl '()) (oldWrds '()) (oldPstns '()) (oldTried '()) (tried '()) (wc (length wrds)) (np '()) (c #t))
    (if (empty? pstns) (writeSol pzzl)
        (begin
          (show pzzl)
          (set! np (replaceWByRCD pzzl (getByRC pstns 0 1)(getByRC pstns 0 2)(getByRC pstns 0 0) (car wrds)))
          (cond
            ((= wc 0) (solveCrssw (car oldPzzl) (sendBack (car oldWrds)) (car oldPstns) (cdr oldPzzl) (cdr oldWrds) (cdr oldPstns) (cdr oldTried) (car oldTried) (- (length (car oldWrds)) 1)))
            ((inList? tried (car wrds))(solveCrssw pzzl (sendBack wrds) pstns oldPzzl oldWrds oldPstns oldTried  tried (- wc 1)))
            ((eq? np #f)(solveCrssw pzzl (sendBack wrds) pstns oldPzzl oldWrds oldPstns oldTried (cons (car wrds) tried) (- wc 1)))
            (else (solveCrssw np (cdr wrds) (cdr pstns) (cons pzzl oldPzzl) (cons wrds oldWrds) (cons pstns oldPstns) (cons (cons (car wrds) tried) oldTried) ))
            )
          )
      )
    )
  )
;declara solution como solucion del crucigrama
;(define solution (solveCrssw puzzle words positions))
(solveCrssw puzzle words positions)
;escribe solucion
;(display positions)(newline)
;(writeSol solution)
