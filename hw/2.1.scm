(define (my-range a b d) ; O(a / d)
  (if (>= a b)
    '()
    (cons a (my-range (+ a d) b d))))

(define (my-flatten xs) ; O(n^2)
  (define (loop xs result)
    (cond ((null? xs) result)
	  ((not (list? (car xs))) (loop (cdr xs) (append result (list (car xs)))))
	  (else (loop (cdr xs) (append result (loop (car xs) '()))))))
  (loop xs '()))

(define (my-flatten-2 xs)
  (define (loop xs accumulator)
    (cond ((and (null? xs) (null? accumulator)) '())
	  ((and (null? xs) (not (null? accumulator))) (loop accumulator '()))
	  ((not (list? (car xs))) (cons (car xs) (loop (cdr xs) accumulator)))
	  ((null? (cdr xs)) (loop (car xs) accumulator))
	  (else (loop (car xs) (cons (cdr xs) accumulator)))))
  (loop xs '()))

(define (my-flatten-3 xs)
  (define (loop xs result)
    (cond ((null? xs) result)
	  ((not (list? (car xs))) (loop (cdr xs) (cons (car xs) result)))
	  (else (loop (cdr xs) (loop (car xs) result)))))
  (reverse (loop xs '())))

(define (my-element? x xs) ; O(n)
  (and (not (null? xs))
       (or (equal? x (car xs))
	   (my-element? x (cdr xs)))))

(define (my-filter pred? xs) ; O(n)
  (define (loop lst)
    (if (null? lst)
      '()
      (if (pred? (car lst))
	(cons (car lst) (loop (cdr lst)))
	(loop (cdr lst)))))
  (loop xs))

(define (my-fold-left op xs) ; O(n)
  (define (loop xs result)
    (if (null? xs)
      result
      (loop (cdr xs) (op result (car xs)))))
  (loop (cdr xs) (car xs)))

(define (my-fold-right op xs) ; O(n)
  (define (loop xs result)
    (if (null? xs)
      result
      (loop (cdr xs) (op (car xs) result))))
  (loop (cdr (reverse xs)) (car (reverse xs))))
