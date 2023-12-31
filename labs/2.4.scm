(define (intersperse e xs)
  (define (loop lst)
    (cond ((null? lst) '())
	  ((null? (cdr lst)) (cons (car lst) '()))
	  (else (cons (car lst) (cons e (loop (cdr lst)))))))
  (loop xs))
(display (intersperse 'x '(1 2 3 4)))
(newline)
(display (intersperse 'x '(1 2)))
(newline)
(display (intersperse 'x '(1)))   
(newline)
(display (intersperse 'x '()))     

