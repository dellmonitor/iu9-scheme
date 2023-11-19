(load "unit-test.scm")

(define (interpret program stack)
  (define (state words word-counter data-stack call-stack dictionary)
    (if (not (equal? (vector-length words) word-counter))
        (cond ((number? (vector-ref words word-counter))
               (state words
                      (+ word-counter 1)
                      (cons (vector-ref words word-counter) data-stack)
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
	))

(run-tests the-tests)

