(define-syntax test
  (syntax-rules ()
    ((test expr res) `(expr res))))

(define (run-test test)
  (let* ((expr (car test))
	 (res (cadr test))
	 (val (eval expr (interaction-environment)))
	 (passed (equal? val res)))
    (begin
      (write expr)
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
