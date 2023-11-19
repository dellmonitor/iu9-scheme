(define (my-gcd a b)
  (if (= b 0)
    (abs a)
    (my-gcd b (remainder a b))))

(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))

(define (smallest-divisor test-divisor n)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (smallest-divisor (next test-divisor) n))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (square x)
  (* x x))

(define (prime? n)
  (and (= (smallest-divisor 2 n) n) (not (= n 1))))
