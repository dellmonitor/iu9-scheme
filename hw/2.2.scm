(define (member? element lst) ; O(n)
  (and (not (null? lst))
       (or (equal? element (car lst))
	   (member? element (cdr lst)))))

(define (list->set lst) ; O(n^2)
  (define (loop lst set)
    (if (null? lst)
      set
      (loop (cdr lst) (if (not (member? (car lst) set))
		       (cons (car lst) set)
		       set))))
  (loop lst '()))

(define (set? lst) ; O(n^2)
  (or (null? lst)
      (and (not (member? (car lst) (cdr lst)))
	   (set? (cdr lst)))))

(define (union xs ys) ; O(n)
  (list->set (append xs ys)))

(define (intersection xs ys) ; O(n)
  (define (loop xs set)
    (if (null? xs)
      set
      (loop (cdr xs) (if (member? (car xs) ys)
		       (cons (car xs) set)
		       set))))
  (loop xs '()))

(define (difference xs ys) ; O(n)
  (define (loop xs set)
    (if (null? xs)
      set
      (loop (cdr xs) (if (member? (car xs) ys)
		       set
		       (cons (car xs) set)))))
  (loop xs '()))

(define (symmetric-difference xs ys) ; O(n)
  (difference (union xs ys) (intersection xs ys)))

(define (set-eq? xs ys) ; O(n)
  (equal? (intersection xs ys) (union xs ys)))
