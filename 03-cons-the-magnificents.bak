;
; Chapter 3 of The Little Schemer:
; Cons the Magnificent
;

; The rember function removes the first occurance of the given atom from the given list.
;
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)           
                  (rember a (cdr lat)))))))

; Examples of rember function
;
(rember 'mint '(lamb chops and mint flavored mint jelly))    ; '(lamb chops and flavored mint jelly)
(rember 'toast '(bacon lettuce and tomato))                  ; '(bacon lettuce and tomato)
(rember 'cup '(coffee cup tea cup and hick cup))             ; '(coffee tea cup and hick cup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
; The second commadment                                                ;
;                                                                      ;
; Use /cons/ to build lists.                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The firsts function builds a list of first s-expressions
;
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (cons (car (car l)) (firsts (cdr l)))))))

; Examples of firsts
;
(firsts '((apple peach pumpkin)
        (plum pear cherry)
        (grape raisin pea)
        (bean carrot eggplant)))       ; '(apple plum grape bean)

(firsts '((a b) (c d) (e f)))          ; '(a c e)
(firsts '((five plums) (four) (eleven green oranges)))    ; '(five foour eleven)
(firsts '(((five plums) four) (eleven green oranges) ((no) more)))    ; '((five plums) eleven (no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                    ;
; The third commadment                                                               ;
;                                                                                    ;
; When building lists, describe the first typical element, and then /cons/           ;
; it onto the natural recursion.                                                     ;
;                                                                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The insertR function inserts the element new to the right of the first
; occurence of element old in the list lat
;
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

; Examples of insertR
;
(insertR
 'jalapeno
 'and
 '(tacos tamales and salsa))        ; '(tacos tamales and jalapeno salsa)

(insertR
 'e
 'd
 '(a b c d f g d h))        ; '(a b c d e f g h)

; The inserL function inserts the elements new to the left of the first
; occurence of element old in the list lat
;
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cons old (cdr lat))))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))

; Example of inserL
;
(insertL
 'd
 'e
 '(a b c e g d h))        ; '(a b c d e g d h)

; The subst function substitutes the first occurrence of element old with new in the list lat
;
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cdr lat)))
       (else
        (cons (car lat) (subst new old (cdr lat)))))))

; Example of subst
;
(subst
 'topping
 'fudge
 '(ice cream with fudge for dessert))        ; '(ice cream with topping for dessert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                         ;
;         Go cons a piece of cake onto your mouth.        ;
;                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The subst2 function substitues the first occurrence of elements o1 or o2
; with new in the list lat
;
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)(eq? (car lat) o2))
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new new o1 o2 (cdr lat)))))))

; Example of subst2
;
(subst2
 'vanilla
 'choclate
 'banana
 '(banana ice cream with chocolate topping))        ; '(vanilla ice cream with chocolate topping)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                                   ;
;           If you got the last function, go and repeat the cake-consing.           ;
;                                                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The multirember function removes all occurrences of a from lat
;
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
        (mutirember a (cdr lat)))
    (else
     (cons (car lat) (multirember a (cdr lat)))))))
















