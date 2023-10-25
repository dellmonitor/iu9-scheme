;;(define (my-even? x)
;;  (= (remainder x 2) 0))

(define (my-even? x)
  (= (/ x 2)
     (quotient x 2)))

(define (my-odd? x)
  (not (my-even? x)))
