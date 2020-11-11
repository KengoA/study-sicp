; Exercise 2.01 (P118)
;
; Define a better version of make-rat that handles both positive and negative arguments.
; make-rat should normalize the sign so that if the rational number is positive, both the numerator and
; denominator are positive, and if the rational number is negative, only the numerator is negative. 

; gcd implementation from P63
(define (gcd a b)
    if (= b 0)
    a
    (gcd b (remainder a b)))

(define (make-rat n d)
    (let ((g (gcd (n d)))))
        (if (< (/ n d) 0)
            (cons (/ (- n) g) (/ d g))
            (cons (/ n g) (/ d g))))

; TEST
(print (make-rat -2 -4))