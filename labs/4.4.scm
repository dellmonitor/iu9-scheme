(load "unit-test.scm")

(define-syntax my-if
  (syntax-rules ()
    ((my-if predicate consequent alternative)
     (let* ((then-expr (delay consequent))
	    (else-expr (delay alternative)))
       (force (or (and predicate then-expr)
	   else-expr))))))

(define the-tests
  (list (test (my-if #t 1 (/ 1 0)) 1)
	(test (my-if #f (/ 1 0) 1) 1)))

(run-tests the-tests)
 