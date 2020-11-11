; Exercise 2.21: The procedure square-list takes a list of numbers as argument and returns a list of the
; squares of those numbers. Here are two different definitions of square-list. Complete both of them by
; filling in the missing expressions: (P145)
(define nil '())

(define (square x)
    (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
; TEST
(print (square-list (list 1 2 3 4)))

(define (square-list items)
  (map square items))

; TEST
(print (square-list (list 1 2 3 4)))