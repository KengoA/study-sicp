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
    (define (addend s)
        (cadr s)
    )
    (define (augend s)
        (if (null? (cdddr s))
            (caddr s)
            (cons '+ (cddr s))
        )
    )
    (define (make-sum a1 a2)
        (cond
            ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))
        )
    )
    (put 'deriv '+ deriv-sum) 
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
        (cond
            ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))
        )
    )
    (define (multiplier p)
        (cadr p)
    )
    (define (multiplicand p) 
        (if (null? (cdddr p))
            (caddr p)
            (cons '* (cddr p))
        )
    )
    (put 'deriv '* deriv-product)
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