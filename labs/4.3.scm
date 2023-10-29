(define (memoize f)
  (let ((known-results '()))
    (lambda (x)
      (let ((previously-computed-result (assoc x known-results)))
	(if previously-computed-result
	  (cadr previously-computed-result)
	  (let ((result (f x)))
	    (set! known-results (cons (list x result) known-results))
	    result))))))

(define memo-trib
  (memoize (lambda (n)
	     (cond ((<= n 1) 0)
		   ((= n 2) 1)
		   (else (+ (memo-trib (- n 1))
			    (memo-trib (- n 2))
			    (memo-trib (- n 3))))))))
