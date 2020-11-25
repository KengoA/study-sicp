; utils
(define (=number? exp num)
    (and
        (number? exp)
        (= exp num)
    )
)

(define (variable? x)
    (symbol? x)
)

(define (same-variable? v1 v2)
    (and
        (variable? v1)
        (variable? v2)
        (eq? v1 v2)
    )
)

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)

; (define (sum? x)
;     (and
;         (pair? x)
;         (eq? (car x) '+)
;     )
; )

; (define (product? x)
;     (and
;         (pair? x)
;         (eq? (car x) '*)
;     )
; )

; (define (exponentiation? x)
;     (and
;         (pair? x)
;         (eq? (car x) '**)
;     )
; )


; packages
(define (install-sum-package)
    (define (deriv-sum exp var) 
        (make-sum
            (deriv (addend exp) var) 
            (deriv (augend exp) var)
        )
    )
    (define (make-sum a1 a2)
        (cons a1 a2)
    )
    ; internal
    (define (tag x)
        (attach-tag '+ x)
    )
    (define (addend s)
        (car s)
    )
    (define (augend s)
        (if (null? (cddr s))
            (cadr s)
            (tag (cdr s))
        )
    )

    (put 'deriv '(+) deriv-sum)
    (put 'make-sum '+
		(lambda (x y)
			(cond
				((=number? x 0) y)
			    ((=number? y 0) x)
			    (else (tag (make-sum x y)))
			)
		)
	)
    'done
)

(define (make-sum x y) 
    ((get 'make-sum '+) x y)
) 

(define (install-product-package)
    (define (deriv-product operands var)
        (make-sum
            (make-product
                (multiplier operands)
                (deriv
                    (multiplicand operands)
                    var
                )
            )
            (make-product
                (deriv
                    (multiplier operands)
                    var
                )
                (multiplicand operands)
            )
        )
    )
    (define (make-product m1 m2)
        (cons m1 m2)
    )
    (define (tag x)
        (attach-tag '* x)
    )
    (define (multiplier p)
        (car p)
    )
    (define (multiplicand p) 
        (if (null? (cddr p))
            (cadr p)
            (tag (cdr p))
        )
    )
    (put 'deriv '(*) deriv-product)
    (put 'make-product '*
		(lambda (x y)
            (cond
                ((or
                    (=number? m1 0)
                    (=number? m2 0))
                    0)
                ((=number? m1 1) m2)
                ((=number? m2 1) m1)
                ((and (number? m1) (number? m2)) (* m1 m2))
                (else (tag (m1 m2)))
            )
        )
	)
)

(define (make-product x y)
    ((get 'make-product '*) x y)
)

(define (install-exponentiation-package)
    (define (deriv-exponentiation operands var)
        (make-product
            (exponent operands)
            (make-product
                (make-exponentiation
                    (base operands)
                    (- (exponent exp) 1)
                )
                (deriv (base exp))
            )
        )
    )
    (define (tag x)
        (attach-tag '** x)
    )
    (define (base e)
        (cadr e)
    )
    (define (exponent e)
        (caddr e)
    )
    (define (make-exponentiation base exponent)
        (cond
            ((eq? exponent 0) 1)
            ((eq? exponent 1) base)
            (else (list '** base exponent))
        )
    )
    (put 'deriv '** deriv-exponentiation)
)

; derive (from P249)
(define (deriv exp var) 
    (cond
        ((number? exp)
            0
        ) 
        ((variable? exp)
            (if
                (same-variable? exp var)
                1
                0
            )
        ) 
        (else ((get 'deriv (operator exp))
               (operands exp) var)
        )
    )
)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp)) 