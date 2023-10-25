(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (let* ((display-expr (display `expr))
	    (display-arrow (display " => "))
	    (value expr)
	    (write-value (write value))
	    (new-line (newline)))
       value))))
