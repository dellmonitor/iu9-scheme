(define (my-fold-left op xs)
  (define (loop xs result)
    (if (null? xs)
      result
      (loop (cdr xs) (op result (car xs)))))
  (loop (cdr xs) (car xs)))

(define (o . fns)
  (define (loop lst)
    (if (null? lst)
      (lambda (x) x)
      (lambda (x) ((car lst) ((loop (cdr lst)) x)))))
  (loop fns))

(define (o-2 . fns)
  (define (custom-eval x op)
    (eval (list op x) (interaction-environment)))
  (lambda (x) (my-fold-left custom-eval (cons x (reverse fns)))))
