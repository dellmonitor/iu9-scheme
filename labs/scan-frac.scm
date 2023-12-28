(load "stream.scm")
(load "unit-test.scm")

; <fraction> ::= SIGN <number0> BAR <number0> | <number0> BAR <number0>
; <number0> ::= DIGIT <number1>
; <number1> ::= DIGIT <number1> | <empty>
; <empty> ::=

(define (scan-frac str)
  (let* ((EOF #\©)
	 (stream (make-stream (string->list str) EOF)))
    (call-with-current-continuation
      (lambda (error)
	(let ((result (fraction stream error)))
	  (and (equal? (peek stream) EOF) result))))))

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
    (test (scan-frac "1/2") 1/2)
    (test (scan-frac "-1/2") -1/2)
    (test (scan-frac "-10/2") -5)
    (test (scan-frac "/") #f)
    (test (scan-frac "1/") #f)
    (test (scan-frac "/2") #f)
    (test (scan-frac "1.0/2") #f)
    (test (scan-frac "F0/2") #f)
    (test (scan-frac "1/0") "Division by zero")
    (test (scan-frac "+1/0") "Division by zero")
    (test (scan-frac "") #f)
    (test (scan-frac "-") #f)
    (test (scan-frac "-/") #f)
    (test (scan-frac "-/2") #f)
    (test (scan-frac "-1/") #f)
    ))

(run-tests the-tests)
