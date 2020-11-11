; Exercise 2.02 (P121)
;
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and an ending point. Define a constructor
; make-segment and selectors start-segment and end-segment that define the representation of
; segments in terms of points. 

(define (make-segment start end)
    (cons start end))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

; Furthermore, a point can be represented as a pair of numbers: the x
; coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors
; x-point and y-point that define this representation. Finally, using your selectors and constructors,
; define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint
; (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures,
; you'll need a way to print points:

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (average a b)
    (/ (+ a b) 2)
)

(define (midpoint-segment s)
    (make-point
        (average (x-point (start-segment s)) (x-point (end-segment s)))
        (average (y-point (start-segment s)) (y-point (end-segment s)))))

; From P121
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))


; TEST
(define start (make-point 0 -10))
(define end (make-point 10 0))

(define seg (make-segment start end))

; Should be (5, -5)
(print-point (midpoint-segment seg))

