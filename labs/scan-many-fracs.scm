(load "stream.scm")
(load "unit-test.scm")

; <fraction> ::= SIGN <number0> BAR <number0> | <number0> BAR <number0>
; <number0> ::= DIGIT <number1>
; <number1> ::= DIGIT <number1> | <empty>
; <empty> ::=

; <fractions0> ::= <fraction> <fractions1> | <spaces> <fractions0>
; <fractions1> ::= <fraction> <fractions1> | <spaces> <fraction1> | empty
; <spaces> ::= SPACE <spaces> | <empty>

(define (scan-many-fracs str)
  (let* ((EOF #\©)
	 (stream (make-stream (string->list str) EOF)))
    (call-with-current-continuation
      (lambda (error)
	(let ((result (fractions0 stream error)))
	  (and (equal? (peek stream) EOF) result))))))

(define (fractions0 stream error)
  (cond ((start-char? (peek stream))
	 (cons (fraction stream error)
	       (fractions1 stream error)))
	((char-whitespace? (peek stream))
	 (spaces stream error)
	 (fractions0 stream error))
	(else (error #f))))

(define (fractions1 stream error)
  (cond ((start-char? (peek stream))
	 (cons (fraction stream error)
	       (fractions1 stream error)))
	((char-whitespace? (peek stream))
	 (spaces stream error)
	 (fractions1 stream error))
	(else '())))

(define (spaces stream error)
  (cond ((char-whitespace? (peek stream))
	 (next stream)
	 (spaces stream error))
	(else #t)))

  (define (start-char? char)
    (or (sign? char) (digit? char)))

(define (fraction stream error)
  (cond ((sign? (peek stream))
	 (let ((sign (if (equal? (next stream) #\+) 1 -1))
	       (numerator-value (number0 stream error)))
	   (cond ((bar? (peek stream))
		  (next stream)
		  (let ((denominator-value (number0 stream error)))
		    (if (= denominator-value 0)
		      (error "Division by zero")
		      (/ numerator-value denominator-value sign))))
		 (else (error #f)))))
	((digit? (peek stream))
	 (let ((numerator-value (number0 stream error)))
	   (cond ((bar? (peek stream))
		  (next stream)
		  (let ((denominator-value (number0 stream error)))
		    (if (= denominator-value 0)
		      (error "Division by zero")
		      (/ numerator-value denominator-value))))
		 (else (error #f)))))
	 (else (error #f))))

(define (number0 stream error)
  (cond ((digit? (peek stream))
	 (let ((digit (custom-char->integer (next stream))))
	   (number1 stream error digit)))
	(else (error #f))))

(define (number1 stream error number)
  (cond ((digit? (peek stream))
	 (let ((next-digit (next stream)))
	   (number1 stream error (+ (* 10 number) (custom-char->integer next-digit)))))
	(else number)))

(define (sign? char)
  (or (equal? char #\+)
      (equal? char #\-)))

(define (bar? char)
  (equal? char #\/))

(define digit? char-numeric?)

(define (custom-char->integer char)
  (- (char->integer char) 48))

(define the-tests
  (list
    (test (scan-many-fracs "\t1/2 1/3\n\n10/8") (1/2 1/3 5/4))
    (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") #f)
    (test (scan-many-fracs "") #f)
    (test (scan-many-fracs " ") #f)
    (test (scan-many-fracs "/2") #f)
    (test (scan-many-fracs "2/") #f)
    ))

(run-tests the-tests)
