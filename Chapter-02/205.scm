; Exercise 2.5: Show that we can represent pairs of nonnegative integers using only
; numbers and arithmetic operations if we represent the pair a and b as the integer
; that is the product 2a 3b. Give the corresponding definitions of the procedures cons, car, and cdr.

; credits to @sh1doy

(define (pow b n)
	(define (iter result count)
		(if (= count 0)
		    result
		    (iter (* result b) (- count 1))
		)
	)
	(iter 1 n)
)
(define (count-base num base)
	(define (iter quotient count)
		(if (= (mod quotient base) 0)
		    (iter (/ quotient base) (+ count 1))
		    count
		)
	)
	(iter num 0)
)
(define (another-cons a b)
	(*
		(pow 2 a)
		(pow 3 b)
	)
)
(define (another-car num)
	(count-base
		num
		2
	)
)
(define (another-cdr num)
	(count-base
		num
		3
	)
)

; TEST
(define num (another-cons 5 4))
(print (count-base num 2))
(print (count-base num 3))