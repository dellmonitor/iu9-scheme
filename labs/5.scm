(load "unit-test.scm")


(define (interpret program stack)
  (define standard-dictionary
    (list 'drop 'swap 'dup 'over 'rot 'depth '+ '- '* '/ 'mod 'neg '= '> '< 'not 'and 'or))

  (define (drop stack)
    (cdr stack))

  (define (swap stack)
    (cons (car (cdr stack))
          (cons (car stack)
                (cdr (cdr stack)))))

  (define (dup stack)
    (cons (car stack) stack))

  (define (over stack)
    (cons (car (cdr stack))
          stack))

  (define (rot stack)
    (cons (car (cdr (cdr stack)))
          (cons (car (cdr stack))
                (cons (car stack)
                      (cdr (cdr (cdr stack)))))))

  (define (depth stack)
    (cons (length stack) stack))

  (define (add stack)
    (cons (+ (car (cdr stack))
	     (car stack))
	  (cdr (cdr stack))))

  (define (subtract stack)
    (cons (- (car (cdr stack))
	     (car stack))
	  (cdr (cdr stack))))

  (define (multiply stack)
    (cons (* (car (cdr stack))
	     (car stack))
	  (cdr (cdr stack))))

  (define (divide stack)
    (cons (/ (car (cdr stack))
	     (car stack))
	  (cdr (cdr stack))))

  (define (mod stack)
    (cons (modulo (car (cdr stack))
		  (car stack))
	  (cdr (cdr stack))))

  (define (neg stack)
    (cons (- (car stack))
	  (cdr stack)))
  
  (define (equal stack)
    (cons (if (= (car (cdr stack))
		 (car stack))
	    -1
	    0)
	  (cdr (cdr stack))))

  (define (greater stack)
    (cons (if (> (car (cdr stack))
		 (car stack))
	    -1
	    0)
	  (cdr (cdr stack))))

  (define (less stack)
    (cons (if (< (car (cdr stack))
		 (car stack))
	    -1
	    0)
	  (cdr (cdr stack))))

  (define (forth-not stack)
    (cons (if (= (car stack) 0) -1 0)
	  (cdr stack)))

  (define (forth-and stack)
    (cons (if (and (not (= (car (cdr stack)) 0))
		   (not (= (car stack) 0)))
	    -1
	    0)
	  (cdr (cdr stack))))

  (define (forth-or stack)
    (cons (if (or (not (= (car (cdr stack)) 0))
		  (not (= (car stack) 0)))
	    -1
	    0)
	  (cdr (cdr stack))))

  (define (standard-procedure procedure)
    (case procedure
      ('drop drop)
      ('swap swap)
      ('dup dup)
      ('over over)
      ('rot rot)
      ('depth depth)
      ('+ add)
      ('- subtract)
      ('* multiply)
      ('/ divide)
      ('mod mod)
      ('neg neg)
      ('= equal)
      ('> greater)
      ('< less)
      ('not forth-not)
      ('and forth-and)
      ('or forth-or)
      ))

  (define (state words word-counter data-stack call-stack dictionary)
    (if (not (equal? (vector-length words) word-counter))
        (cond ((number? (vector-ref words word-counter))
               (state words
                      (+ word-counter 1)
                      (cons (vector-ref words word-counter) data-stack)
                      call-stack
                      dictionary))
              ((member (vector-ref words word-counter) standard-dictionary)
               (state words
                      (+ word-counter 1)
                      ((standard-procedure (vector-ref words word-counter)) data-stack)
                      call-stack
                      dictionary))
              )
        data-stack))
  (state program 0 stack '() '()))

(define the-tests
  (list (test (interpret #() '()) ())
        (test (interpret #(1) '()) (1))
        (test (interpret #(1 2) '()) (2 1))
        (test (interpret #() '(1)) (1))
        (test (interpret #() '(1 2)) (1 2))
        (test (interpret #(1) '(1)) (1 1))
        (test (interpret #(1 2) '(3)) (2 1 3))
        (test (interpret #(3) '(1 2)) (3 1 2))
        (test (interpret #(1 2) '(3 4)) (2 1 3 4))
        (test (interpret #(drop) '(1)) ())
        (test (interpret #(swap) '(1 2)) (2 1))
        (test (interpret #(dup) '(1 2)) (1 1 2))
	(test (interpret #(over) '(1 2)) (2 1 2))
	(test (interpret #(rot) '(1 2 3)) (3 2 1))
	(test (interpret #(depth) '(1 3)) (2 1 3))
	(test (interpret #(+) '(2 1)) (3))
	(test (interpret #(-) '(2 1)) (-1))
	(test (interpret #(*) '(2 3)) (6))
	(test (interpret #(/) '(2 6)) (3))
	(test (interpret #(mod) '(7 9)) (2))
	(test (interpret #(neg) '(1)) (-1))
	(test (interpret #(=) '(2 2)) (-1))
	(test (interpret #(=) '(2 3)) (0))
	(test (interpret #(>) '(1 2)) (-1))
	(test (interpret #(>) '(3 2)) (0))
	(test (interpret #(<) '(2 1)) (-1))
	(test (interpret #(<) '(1 2)) (0))
	(test (interpret #(not) '(0)) (-1))
	(test (interpret #(not) '(2)) (0))
	(test (interpret #(not) '(-2)) (0))
	(test (interpret #(and) '(0 0)) (0))
	(test (interpret #(and) '(1 0)) (0))
	(test (interpret #(and) '(0 1)) (0))
	(test (interpret #(and) '(-1 1)) (-1))
	(test (interpret #(or) '(0 0)) (0))
	(test (interpret #(or) '(1 0)) (-1))
	(test (interpret #(or) '(0 -1)) (-1))
	(test (interpret #(or) '(-1 1)) (-1))
        ))

(run-tests the-tests)

