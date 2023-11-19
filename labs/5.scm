(load "unit-test.scm")


(define (interpret program stack)
  (define standard-dictionary
    (list 'drop 'swap 'dup 'over 'rot 'depth))

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

  (define (standard-procedure procedure)
    (case procedure
      ('drop drop)
      ('swap swap)
      ('dup dup)
      ('over over)
      ('rot rot)
      ('depth depth)
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
        ))

(run-tests the-tests)

