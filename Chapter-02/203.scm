; Exercise 2.3: Implement a representation for rectangles in a plane. 
; (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors,
; create procedures that compute the perimeter and the area of a given rectangle.
; Now implement a different representation for rectanles. Can you design your system with suitable abstraction
; barriers, so that the same perimeter and area procedures will work using either representation? (P122)

(load 202.scm)

(define (square x)
  (* x x))

(define (make-rectangle top-left down-right)
    (let ((top-right (make-point (x-point down-right) (y-point top-left)))
    	  (down-left (make-point (x-point top-left) (y-point down-right))))
    (cons-sides top-left down-left down-right top-right)))

(define (cons-sides top-left down-left down-right top-right)
    (let ((top (make-segment top-left top-right))
          (down (make-segment down-left down-right))
          (left (make-segment top-left down-left))
          (right (make-segment top-right down-right)))
    (cons (cons top down) (cons left right))))

(define (top-side r)
    (caar r))

(define (down-side r)
    (cdar r))

(define (left-side r)
    (cadr r))

(define (right-side r)
    (cddr r))

(define (segment-length s)
    (sqrt (+ (square (abs (- (x-point (end-segment s)) (x-point (start-segment s)))))
             (square (abs (- (y-point (end-segment s)) (y-point (start-segment s))))))))

(define (perim r)
    (* 2 (+ (segment-length (left-side r)) (segment-length (right-side r)))))

(define (area r)
    (* (segment-length (left-side r)) (segment-length (top-side r))))

; TEST
(define r (make-rectangle (make-point 0 10) (make-point 10 0)))
; Should be 40
(print (perim r))
; Should be 100 
(print (area r))
