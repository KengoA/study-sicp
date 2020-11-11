; Exercise 2.4: Here is an alternative procedural representation of pairs. 
; For this representation, verify that (car (cons x y)) yields x for any objects x and y. (P125)

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

; TEST
(car cons (0 1))
(cdr cons (0 1))