(load "stream.scm")
(load "unit-test.scm")

; <Program>  ::= <Articles> <Body> .
; <Articles> ::= <Article> <Articles> | .
; <Article>  ::= define word <Body> end .
; <Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define (parse vctr)
  (let* ((EOF #\©)
	 (stream (make-stream (vector->list vctr) EOF)))
    (call-with-current-continuation
      (lambda (error)
	(let ((result (program stream error)))
	  (and (equal? (peek stream) EOF) result))))))

(define (program stream error)
  (list (articles stream error)
	(body stream error)))

(define (articles stream error)
  (cond ((define? (peek stream))
	 (cons (article stream error)
	       (articles stream error)))
	(else '())))

(define (article stream error)
  (cond ((define? (peek stream))
	 (next stream)
	 (cond ((word? (peek stream))
		(let ((lst (list (next stream)
				 (body stream error))))
		  (cond ((end? (peek stream))
			 (next stream)
			 lst)
			(else (error #f)))))
	       (else (error #f))))
	(else '())))

(define (body stream error)
  (cond ((if? (peek stream))
         (cons (cons (next stream)
                     (list (body stream error)))
               (cond ((endif? (peek stream))
                      (next stream)
                      (body stream error))
                     (else (error #f)))))
        ((integer? (peek stream))
         (cons (next stream)
               (body stream error)))
        ((word? (peek stream))
         (cons (next stream)
               (body stream error)))
        (else '())))

(define (define? token)
  (equal? token 'define))

(define (end? token)
  (equal? token 'end))

(define (if? token)
  (equal? token 'if))

(define (endif? token)
  (equal? token 'endif))

(define (word? token)
  (and (not (number? token))
       (case token
	 ((define end if endif #\©) #f)
	 (else #t))))

(define the-tests
  (list
    (test (parse #(1 2 +)) (() (1 2 +)))
    (test (parse #(x dup 0 swap if drop -1 endif 6 7)) (() (x dup 0 swap (if (drop -1)) 6 7)))
    (test (parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial )) (((-- (1 -))
   (=0? (dup 0 =))
   (=1? (dup 1 =))
   (factorial
    (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
  (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)))
    ))
(run-tests the-tests)
