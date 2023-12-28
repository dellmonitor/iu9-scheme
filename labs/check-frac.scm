(load "stream.scm")
(load "unit-test.scm")

; <fraction> ::= SIGN <number0> BAR <number0> | <number0> BAR <number0>
; <number0> ::= DIGIT <number1>
; <number1> ::= DIGIT <number1> | <empty>
; <empty> ::=

(define (check-frac str)
  (let* ((EOF #\©)
	 (stream (make-stream (string->list str) EOF)))
    (call-with-current-continuation
      (lambda (error)
	(fraction stream error)
	(equal? (peek stream) EOF)))))

(define (fraction stream error)
  (cond ((sign? (peek stream))
	 (next stream)
	 (number0 stream error)
	 (cond ((bar? (peek stream))
		(next stream)
		(number0 stream error))
	       (else (error #f))))
	((digit? (peek stream))
	 (number0 stream error)
	 (cond ((bar? (peek stream))
		(next stream)
		(number0 stream error))
	       (else (error #f))))
	(else (error #f))))

(define (number0 stream error)
  (cond ((digit? (peek stream))
	 (next stream)
	 (number1 stream error))
	(else (error #f))))

(define (number1 stream error)
  (cond ((digit? (peek stream))
	 (next stream)
	 (number1 stream error))
	(else #t)))

(define (sign? char)
  (or (equal? char #\+)
      (equal? char #\-)))

(define (bar? char)
  (equal? char #\/))

(define digit? char-numeric?)

(define the-tests 
  (list 
    (test (check-frac "1/2") #t)
    (test (check-frac "-1/2") #t)
    (test (check-frac "/") #f)
    (test (check-frac "1/") #f)
    (test (check-frac "/2") #f)
    (test (check-frac "1.0/2") #f)
    (test (check-frac "F0/2") #f)
    (test (check-frac "1/0") #t)
    (test (check-frac "") #f)
    (test (check-frac "-") #f)
    (test (check-frac "-/") #f)
    (test (check-frac "-/2") #f)
    (test (check-frac "-1/") #f)
    ))

(run-tests the-tests)
