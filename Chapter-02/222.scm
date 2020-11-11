; Exercise 2.22: Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so
; that it evolves an iterative process:
(define nil '())

(define (square-list items)
    (define (iter things answer)
        (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                     answer))))
    (iter items nil))

(print (square-list (list 1 2 3 4)))
; (1 2 3 4)
; (2 3 4) (1 nil)
; (3 4) (4 1 nil)
; (4) (9 4 1 nil)
; () (16 9 4 1 nil)
; (16 9 4 1 nil)

; Unfortunately, defining square-list this way produces the answer list in the reverse order of
; the one desired. Why? Louis then tries to fix his bug by interchanging the arguments to cons:
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                  (square (car things))))))
    (iter items nil))

; This doesn’t work either. Explain. (P145)
(print (square-list (list 1 2 3 4)))
; (nil 1)
; ((nil 1), 4)
; (((nil 1), 4), 9)
; ((((nil 1), 4), 9), 16)
