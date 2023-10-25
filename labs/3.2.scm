(define-syntax test
  (syntax-rules ()
    ((test expr res) `(expr res))))

(define (run-test test)
  (let* ((expr (car test))
	 (res (cadr test))
	 (val (eval expr (interaction-environment)))
	 (passed (equal? val res)))
    (begin
      (display expr)
      (if passed
	(display " ok")
	(begin
	  (display " FAIL") (newline)
	  (display " Expexted: ") (write res) (newline)
	  (display " Returned: ") (write val)))
      (newline)
      passed)))

(define (run-tests test)
  (define (loop lst success)
    (if (null? lst)
        success
        (loop (cdr lst) (and (run-test (car lst)) success))))
  (loop test #t))

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)

