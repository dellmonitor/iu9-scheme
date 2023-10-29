(define (day-of-week day month year)
  (let* ((a (quotient (- 14 month) 12))
	(y (- year a))
	(m (+ month (* 12 a) -2)))
    (remainder (+ day
		  (quotient (* 31 m) 12)
		  y
		  (quotient y 4)
		  (- (quotient y 100))
		  (quotient y 400))
	       7)))


(day-of-week 04 12 1975)
(day-of-week 04 12 2006)
(day-of-week 29 05 2013)
