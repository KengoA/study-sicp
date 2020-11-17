; Exercise 2.64: The following procedure list->tree converts an ordered list to a balanced binary tree.
; The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and
; constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree
; is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.

(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left right)
    (list entry left right)
)

(define (adjoin-set x set)
    (cond
        ((null? set)
            (make-tree x '() '()))
         ((= x (entry set))
            set)
         ((< x (entry set))
            (make-tree
                (entry set)
                (adjoin-set x (left-branch set))
                (right-branch set)))
         ((> x (entry set))
            (make-tree
                (entry set)
                (left-branch set)
                (adjoin-set x (right-branch set))
            )
        )
    )
)

(define (list->tree elements)
    (car
        (partial-tree elements
        (length elements))
    )
)

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let
            ((left-size
             (quotient (- n 1) 2)))
            (let
                ((left-result (partial-tree elts left-size)))
                (let
                    ((left-tree (car left-result))
                     (non-left-elts (cdr left-result))
                     (right-size (- n (+ left-size 1))))
                    (let
                        ((this-entry (car non-left-elts))
                         (right-result
                            (partial-tree
                                (cdr non-left-elts)
                                right-size))    
                        )
                        (let
                            ((right-tree (car right-result))
                                (remaining-elts
                                (cdr right-result)))
                            (cons
                                (make-tree
                                    this-entry
                                    left-tree
                                    right-tree)
                                remaining-elts
                            )
                        )
                    )
                )
            )
        )
    )
)

; TEST
(define ordered_list (list 1 2 3 4 5 6 7 8))
(print (list->tree ordered_list))


; a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).
; Put

; b. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?
; log(n)