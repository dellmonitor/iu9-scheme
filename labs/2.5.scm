(define (any? pred? xs)
  (and (not (null? xs))
       (or (pred? (car xs))
	   (any? pred? (cdr xs)))))

(any? odd? '(1 3 5 7))
(any? odd? '(0 1 2 3))
(any? odd? '(0 2 4 6))
(any? odd? '()) 

(newline)

(define (all? pred? xs)
  (or (null? xs)
       (and (pred? (car xs))
	   (all? pred? (cdr xs)))))

(display (all? odd? '(1 3 5 7)))
(newline)
(display (all? odd? '(0 1 2 3)))
(newline)
(display (all? odd? '(0 2 4 6)))
(newline)
(display (all? odd? '()))
