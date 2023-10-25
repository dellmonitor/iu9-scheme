(define (my-count x xs)
  (define (loop counter lst)
    (if (null? lst)
      counter
      (loop (+ counter (if (equal? (car lst)
				   x)
			 1
			 0)) (cdr lst))))
  (loop 0 xs))

(display (my-count 'a '(a b c a)))
(newline)
(display (my-count 'b '(a c d)))
(newline)
(display (my-count 'a '()))
