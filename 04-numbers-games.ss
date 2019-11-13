;
; Chapter 4 of The Little Schemer:
; Numbers Games
;

; Assume add1 is a primitive
;
(define add1
  (lambda (n) (+ n 1)))

; Example of add1
;
(add1 67)        ;68

; Assume sub1 is a primitive
;
(define sub1
  (lambda (n) (- n 1)))

; Example of sub1
;
(sub1 5)        ;4

; Examples of zero?
;
(zero? 0)        ; true
(zero? 1492)     ; false

; The o+ function adds two numbers
;
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

; Example of o+
;
(o+ 46 12)        ;58

; The o- function substracts one number from other
;
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; Example of o-
;
(o- 14 3)        ; 11
(o- 17 9)        ; 8

; Examples of tups (tup is short for tuple)
;
'(2 111 3 79 47 6)
'(8 55 5 555)
'()

; Examples of non-tups
;
'(1 2 8 apple 4 3)        ; not-a-tup because apple is not a number
'(3 (7 4) 13 9)           ; not-a-tup because (7 4) is a list of numbers, not a number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                          ;
; The first commandment (first revision)                                   ;
;                                                                          ;
; When recurring on a list of atoms, lat, ask two questions about it:      ;
; (null? lat) and else.                                                    ;
; When recurring on a number, n, ask two questions about it: (zero? n)     ;
; and else.                                                                ;
;                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The addtup function adds all numbers in a tup
;
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

; Examples of addtup
;
(addtup '(3 5 2 8))        ; 18
(addtup '(15 6 7 12 3))    ; 43
 ; The o* function multiplies two numbers
;
(define o*
  (lambda (n m)
    (cond
      ((zero? m)0)
      (else (o+ n (o* n (sub1 m)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                          ;
; The fourth commandment (first revision)                                  ;
;                                                                          ;
; Always change at least one argument while recurring. It must be changed  ;
; to be closer to termination. The changing argument must be tested in the ;                                                   ;
; termination condition                                                    ;
;                                                                          ;
; when using cdr, test the termination with null?                          ;
; and when using sub1, test termination with zero?                         ;
;                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Examples of o*
;
(o* 5 3)        ; 15
(o* 13 4)       ; 52

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                          ;
; The fifth commandent                                                                     ;
;                                                                                          ;      
; When building a value with o+, always use 0 for the value of the terminating line,       ;
; for adding 0 does not change the value of an addition.                                   ;
;                                                                                          ;
; When building a value with o*, always use 1 for the value of the terminating line,       ;
; for multiplying by 1 does not change the value of a multiplication.                      ;
;                                                                                          ;
; When building a value with cons, always consider () for the value of the terminating     ;
; line.                                                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The tup+ function adds two tups
;
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1)(car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

; Examples of tup+
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))    ; '(11 11 11 11 110
(tup+ '(3 7) '(4 6 8 1))             ; '(7 13 8 1)

; The o> function compares n with m and returns true if n>m
;
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (o> (sub1 n)(sub1 m))))))

; Examples of o>
;
(o> 12 133)    ; #f
(o> 120 11)    ; #t
(o> 6 6)       ; #f

; The o< function compares n with m and returns true if n<m
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (o< (sub1 n)(sub1 m))))))

; Examples of o<
;
(o< 4 6)    ; #t
(o< 8 3)    ; #f
(o< 6 6)    ; #f















