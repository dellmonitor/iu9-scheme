(define r #f)

(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertions)
     (call-with-current-continuation
      (lambda (c)
        (set! r c))))))

(define-syntax assert
  (syntax-rules ()
    ((assert condition)
     (if (not (eval condition (interaction-environment)))
         (begin
           (display "FAILED: ")
           (r (display 'condition)))))))
(use-assertions)
(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))
    
