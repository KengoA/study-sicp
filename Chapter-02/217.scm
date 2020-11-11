; Exercise 2.17: Define a procedure last-pair that returns the list that contains
; only the last element of a given (nonempty) list:

(define (last-pair items)
  (let ((last (cdr items)))
    (if (null? last)
        items
        (last-pair last))))

; TEST
; Should be 34
(print last-pair (list 23 72 149 34))