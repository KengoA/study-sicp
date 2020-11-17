; Exercise 2.62: Give a Θ(n) implementation of union-set for sets represented as ordered lists.

(define (element-of-set? x set)
    (cond
        ((null? set)
            #f)
        ((= x (car set))
            #t)
        ((< x (car set))
            #f)
        (else (element-of-set? x (cdr set)))
    )
)

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond
                ((= x1 x2)
                    (cons
                        x1
                        (intersection-set (cdr set1) (cdr set2))
                    )
                )
                ((< x1 x2)
                    (intersection-set (cdr set1) set2)
                )
                ((< x2 x1)
                    (intersection-set set1 (cdr set2))
                )
            )
        )
    )
)

(define (union-set set1 set2)
    (define (union-iter s1 s2 accumulate)
        (cond
            ((null? s1)
                (append accumulate s2)
            )
            ((null? s2)
                (append accumulate s1)
            )
            ((< (car s1) (car s2))
                (union-iter
                    (cdr s1)
                    s2
                    (append
                        accumulate
                        (list (car s1))
                    )
                )
            )
            ((> (car s1) (car s2))
                (union-iter
                    s1
                    (cdr s2)
                    (append
                        accumulate
                        (list (car s2))
                    )
                )
            )
            (else
                (union-iter
                    (cdr s1)
                    (cdr s2)
                    (append
                        accumulate 
                        (list (car s1))
                    )
                )
            )
        )
    )
    (union-iter set1 set2 '()))


; TEST
(define set1 (list 1 2 3 4 5))
(define set2 (list 4 5 6 7 8))
(print (union-set set1 set2))