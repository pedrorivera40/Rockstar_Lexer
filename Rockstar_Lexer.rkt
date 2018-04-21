#lang racket

;This project is requisite for the ICOM4036 (Programming Languages) course, and consists on the implementation of a lexical analyzer that follows this set of rulse:
; 1) identifiers: [a-zA-Z]+
; 2) delimiters: "("|")"|”[“|”]”
; 3) operators: "+"|"*"|”%”
; 4) integers: -?[0-9]+
; 5) whitespace: [ \n]+
; SAMPLE:
;   (INPUT) (rockstar_lexer "2*[6%2]")
;   (OUTPUT) ((INT 2) (OP *) (LB) (INT 6) (OP module) (INT 2) (RB))
;@Author Pedro Luis Rivera

; MAIN Lexer Call...
(define (rockstar_lexer code_string)
  (display (append_char_list (merge_alphabetic (merge_whitespace (merge_integers (categorize (string->list code_string)))))))
 )

; Tokenize each character
(define (categorize code)
  (cond
    ((null? code) '())
    ((and (= 1 (length code)) (char=? (car code) #\-)) (cons '(? #\-) (categorize (cdr code))))
    ((is_delimiter (car code)) (cons (is_delimiter (car code)) (categorize (cdr code))))
    ((char-alphabetic? (car code)) (cons (list 'ID (car code)) (categorize (cdr code))))
    ((char-numeric? (car code)) (cons (list 'INT (car code)) (categorize (cdr code))))
    ((char=? (car code) #\-) (if (char-numeric? (car (cdr code))) (cons '(INT #\-) (categorize (cdr code))) (cons '(? #\-) (categorize (cdr code)))))
    ((is_operator (car code)) (cons (is_operator (car code)) (categorize (cdr code))))
    ((is_whitespace (car code)) (cons (is_whitespace (car code)) (categorize (cdr code))))
    (else (cons (list '? (car code)) (categorize (cdr code))))
   )
 )

; Merge each consecutive integer tuple...
(define (merge_integers tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((eq? '() (cdr tuples)) (car tuples))
    ((and (eq? 'INT (car (car tuples))) (eq? 'INT (car (car (cdr tuples)))) (not (eq? #\- (car (cdr (car (cdr tuples))))))) (merge_integers (cons (cons 'INT (append (cdr (car tuples)) (cdr (car (cdr tuples))))) (cdr (cdr tuples)))))
    (else (cons (car tuples) (merge_integers (cdr tuples))))
  )
 )

; Merge each consecutive character tuple...
(define (merge_alphabetic tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((eq? '() (cdr tuples)) (car tuples))
    ((and (eq? 'ID (car (car tuples))) (eq? 'ID (car (car (cdr tuples))))) (merge_alphabetic (cons (cons 'ID (append (cdr (car tuples)) (cdr (car (cdr tuples))))) (cdr (cdr tuples)))))
    (else (cons (car tuples) (merge_alphabetic (cdr tuples))))
  )
 )

; Merge each consecutive whitespace...
(define (merge_whitespace tuples)
  (cond
    ((null? tuples) '())
    ((= 1 (length tuples)) tuples)
    ((and (eq? 'WS (car (car tuples))) (eq? 'WS (car (car (cdr tuples))))) (merge_whitespace (cdr tuples)))
    (else (cons (car tuples) (merge_whitespace (cdr tuples))))
  )
 )

; For every tuple, refactor as a valid tuple (for display purposes...)
(define (append_char_list tuples)
  ;tuples
  (cond
   ((null? tuples) '())
   (else (cons (tuple_refactor (car tuples)) (append_char_list (cdr tuples))))
  )
  )

; Converts the list of values into a string (if is a single value, convert it to list and then to string...)
(define (tuple_refactor tuple)
  (cond
    ((null? tuple) '())
    ((= 1 (length tuple)) tuple)
    ((= 1 (length (cdr tuple))) (list (car tuple) (list->string (cdr tuple))))
    (else (list (car tuple) (list->string (cdr tuple))))
  )
 )

; Validate if is a valid operator...
(define (is_operator c)
  (cond
    ((char=? c #\+) '(OP #\+))
    ((char=? c #\*) '(OP #\*)) 
    ((char=? c '#\%) '(OP #\m #\o #\d #\u #\l #\e))
    (else #F)
   )
)

; Validate if is a valid delimiter...
(define (is_delimiter c)
  (cond
    ((char=? c #\() '(LP))
    ((char=? c #\)) '(RP))
    ((char=? c '#\[) '(LB))
    ((char=? c '#\]) '(RB))
    (else #F)
   )
)

; Validate if is a whitespace...
(define (is_whitespace c)
  (if (char-whitespace? c) '(WS) #F)
)
