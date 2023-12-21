(define e (exp 1))

(define (square x)
  (* x x))

(define (derivative expression)
  (cond 
    ((number? expression) 0)

    ((and (list? expression)
          (equal? (car expression) '+))
     (if (= (length expression) 3)
         (list '+ (derivative (car (cdr expression))) (derivative (car (cdr (cdr expression)))))
         (list '+ (derivative (car (cdr expression))) (derivative (cons '+ (cdr (cdr expression)))))))

    ((and (list? expression)
          (equal? (car expression) '-))
     (cond 
       ((= (length expression) 2) (list '- (derivative (car (cdr expression)))))
       ((= (length expression) 3) (list '- 
                                        (derivative (car (cdr expression)))
                                        (derivative (car (cdr (cdr expression))))))
       (else (list '- 
                   (derivative (car (cdr expression))) 
                   (derivative (cons '+ (cdr (cdr expression))))))))

    ((and (list? expression)
          (equal? (car expression) '*))
     (if (= (length expression) 3)
         (list '+ (list '* 
                        (derivative (car (cdr expression))) 
                        (car (cdr (cdr expression)))) 
               (list '* (car (cdr expression)) (derivative (car (cdr (cdr expression))))))
         (list '+
               (list '* 
                     (derivative (car (cdr expression))) 
                     (cons '* (cdr (cdr expression)))) 
               (list '* (car (cdr expression)) (derivative (cons '* (cdr (cdr expression))))))))

    ((and (list? expression)
          (equal? (car expression) '/))
     (if (= (length expression) 3)
         (list '/ (list '- 
                        (list '* 
                              (derivative (car (cdr expression))) 
                              (car (cdr (cdr expression)))) 
                        (list '* 
                              (car (cdr expression)) 
                              (derivative (car (cdr (cdr expression)))))) 
               (list 'square (car (cdr (cdr expression)))))
         (derivative (list '/ (car (cdr expression)) (cons '* (cdr (cdr expression)))))))

    ((and (list? expression)
          (equal? (car expression) 'expt))
     (cond ((equal? (car (cdr expression)) 'e)
            (list '* expression (derivative (cons '* (cons 1 (cdr (cdr expression))))))) ; костыль
           ((number? (car (cdr (cdr expression))))
            (list '* 
                  (car (cdr (cdr expression))) 
                  (list 'expt 
                        (car (cdr expression)) 
                        (list '- (car (cdr (cdr expression))) 1)) 
                  (derivative (car (cdr expression)))))
           (else (derivative (list 'expt 
                                   'e 
                                   (list '* 
                                         (cons '* (cons 1 (cdr (cdr expression)))) 
                                         (list 'log (car (cdr expression)))))))))

    ((and (list? expression)
          (equal? (car expression) 'log))
     (if (= (length expression) 2)
         (list '* (list '/ 1 (cons '* (cdr expression))) (derivative (cons '* (cons 1 (cdr expression)))))
         (list '* (list '/ 1 (list '* 
                                   (car (cdr expression)) 
                                   (list 'log 
                                         (cons '* (cons 1 (cdr (cdr expression))))))))))

    ((and (list? expression)
          (equal? (car expression) 'sin))
     (list '* (list 'cos (car (cdr expression))) (derivative (car (cdr expression)))))

    ((and (list? expression)
          (equal? (car expression) 'cos))
     (list '* (list '- (list 'sin (car (cdr expression)))) (derivative (car (cdr expression)))))

    ((and (list? expression)
          (or (equal? (car expression) 'tg)
              (equal? (car expression) 'tan))
          (list '* (list '/ 1 
                         (list 'square
                               (list 'cos 
                                     (car (cdr expression))))) 
                (derivative (car (cdr expression))))))

    ((and (list? expression)
          (or (equal? (car expression) 'ctg)
              (equal? (car expression) 'cot)))
     (list '* 
	   (list '- (list '/ 1 (list 'square (list 'sin (car (cdr expression)))))) 
	   (derivative (car (cdr expression)))))

    ((and (list? expression)
          (equal? (car expression) 'exp))
     (list '* expression (derivative (cons '* (cons 1 (cdr expression))))))

    (else 1)
    ))
