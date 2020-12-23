(define (make-accumulator acc)
    (lambda (x)
        (set! acc (+ x acc))
        acc
    )
)