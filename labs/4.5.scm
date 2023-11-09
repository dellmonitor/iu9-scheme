(load "unit-test.scm")
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var expr) ...)
	     body)
     ((lambda (var ...)
	body)
      expr ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((name1 value1)) expr1 ...)
     (my-let ((name1 value1)) expr1 ...))
    ((my-let* ((name1 value1) (name2 value2) ...) expr1 ...)
     (my-let ((name1 value1))
             (my-let* ((name2 value2) ...) expr1 ...)))))

(define let-tests
  (list
   (test (my-let () 10) 10)
   (test (my-let ((x 100)) x) 100)
   (test (my-let ((x 10) (y (* x 10))) (+ x y)) 110)))

(run-tests let-tests)


