(load "unit-test.scm")

(define-syntax my-if
  (syntax-rules ()
    ((my-if predicate consequent alternative)
     (let* ((then-expr (delay consequent))
	    (else-expr (delay alternative)))
       (or (and predicate (force then-expr))
	   (force else-expr))))))

(define the-tests
  (list (test (my-if #t 1 (/ 1 0)) 1)
	(test (my-if #f (/ 1 0) 1) 1)))

(run-tests the-tests)
