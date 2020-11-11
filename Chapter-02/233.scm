; Exercise 2.33: Fill in the missing expressions to complete the following definitions of
; some basic list-manipulation operations as accumulations: (P161)

(define nil '())
(define (square x)
    (* x x))

; From P158
(define (accumulate op initial sequence) (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; TEST
(define seq1 (list 1 2 3 4))
(define seq2 (list 5 6 7 8 9))

; Should be (1 4 9 16)
(print (map square seq1))
; Should be (1 2 3 4 5 6 7 8 9)
(print (append seq1 seq2))
; Should be 4
(print (length seq1))
; Should be 5
(print (length seq2))


