;
; Chapter 2 of The Little Schemer 4th
; Do it, Do it Again, and Again, and Again ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We need to define atom? for Scheme as it's not a primitive           ;
;                                                                      ;
(define atom?                                                          ;
  (lambda (x)                                                          ;
     (and (not (pair? x))(not (null? x)))))                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; lat? function fins if all the elements in the lists are atoms
; (lat stands for list of atoms)
;
(define lat?
  (lambda (l)
      (cond
        ((null? l) #t)
        ((atom? (car l)) (lat? (cdr l)))
        (else #f))))

; Examples of lats:
;
(lat? '(Jack Sprat could eat no chicken fat))
(lat? '())
(lat? '(bacon and eggs))

; Examples of not-lats:
;
(lat? '((Jack) Sprat could eat no chicken fat))    ; not-lat because (car l) is a list
(lat? '(Jack (Sprat could) eat no chicken fat))    ; not-lat because l contains a list
(lat? '(bacon (and eggs)))                         ; not-lat because '(and eggs) is alist

; Examples of or:
;
(or (null? '()) (atom? '(d e f g)))                ; true
(or (null? '(a b c)) (null? '()))                  ; true
(or (null? '(a b c)) (null? '(atom)))              ; false

; member? function determines if an elements is in a lat (list of atoms)
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                     (member? a (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
; The first commandment (preliminiary)                                   ;
;                                                                        ;
; Always ask /null?/ as the first question in expressing any function.   ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Examples of member? succeeding
;
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'meat '(potatoes and meat gracy))
(member? 'meat '(and meat gracy))
(member? 'meat '(meat gracy))

; Examples of member? failing
(member? 'liver '(bagels and lox))
(member? 'liver '())


