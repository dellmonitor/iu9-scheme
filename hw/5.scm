(load "../labs/unit-test.scm")


(define (interpret program stack)

  (define standard-dictionary
    (list 'drop 'swap 'dup 'over 'rot 'depth '+ '- '* '/ 'mod 'neg '= '> '< 'not 'and 'or))

  (define (drop stack) ; Удаляет элемент на вершине стека. (n1) → ()
    (cdr stack))

  (define (swap stack) ; Меняет местами два элемента на вершине стека. (n2 n1) → (n1 n2)
    (cons (car (cdr stack))
          (cons (car stack)
                (cdr (cdr stack)))))

  (define (dup stack) ; Дублирует элемент на вершине стека. (n1) → (n1 n1)
    (cons (car stack) stack))

  (define (over stack) ; Копирует предпоследний элемент на вершину стека. (n2 n1) → (n1 n2 n1)
    (cons (car (cdr stack))
          stack))

  (define (rot stack) ; Меняет местами первый и третий элемент от головы стека. (n3 n2 n1) → (n1 n2 n3)
    (cons (car (cdr (cdr stack)))
          (cons (car (cdr stack))
                (cons (car stack)
                      (cdr (cdr (cdr stack)))))))

  (define (depth stack) ; Возвращает число элементов в стеке перед своим вызовом. (...) → (n ...)
    (cons (length stack) stack))

  (define (add stack) ; Сумма n1 и n2. (n2 n1) → (сумма)
    (cons (+ (car (cdr stack))
             (car stack))
          (cdr (cdr stack))))

  (define (subtract stack) ; Разность: n1 − n2. (n2 n1) → (разность)
    (cons (- (car (cdr stack))
             (car stack))
          (cdr (cdr stack))))

  (define (multiply stack) ; Произведение n1 на n2. (n2 n1) → (произведение)
    (cons (* (car (cdr stack))
             (car stack))
          (cdr (cdr stack))))

  (define (divide stack) ; Целочисленное деление n1 на n2. (n2 n1) → (частное)
    (cons (/ (car (cdr stack))
             (car stack))
          (cdr (cdr stack))))

  (define (mod stack) ; Остаток от деления n1 на n2. (n2 n1) → (остаток)
    (cons (modulo (car (cdr stack))
                  (car stack))
          (cdr (cdr stack))))

  (define (neg stack) ; Смена знака числа. (n) → (−n)
    (cons (- (car stack))
          (cdr stack)))
  
  (define (equal stack) ; Флаг равен −1, если n1 = n2, иначе флаг равен 0. (n2 n1) → (флаг)
    (cons (if (= (car (cdr stack))
                 (car stack))
              -1
              0)
          (cdr (cdr stack))))

  (define (greater stack) ; Флаг равен −1, если n1 > n2, иначе флаг равен 0. (n2 n1)
    (cons (if (> (car (cdr stack))
                 (car stack))
              -1
              0)
          (cdr (cdr stack))))

  (define (less stack) ; Флаг равен −1, если n1 < n2, иначе флаг равен 0. (n2 n1)
    (cons (if (< (car (cdr stack))
                 (car stack))
              -1
              0)
          (cdr (cdr stack))))

  (define (forth-not stack) ; НЕ n. (n) → (результат)
    (cons (if (= (car stack) 0) -1 0)
          (cdr stack)))

  (define (forth-and stack) ; n2 И n1. (n2 n1) → (результат)
    (cons (if (and (not (= (car (cdr stack)) 0))
                   (not (= (car stack) 0)))
              -1
              0)
          (cdr (cdr stack))))

  (define (forth-or stack) ; n2 ИЛИ n1. (n2 n1) → (результат)
    (cons (if (or (not (= (car (cdr stack)) 0))
                  (not (= (car stack) 0)))
              -1
              0)
          (cdr (cdr stack))))

  (define (standard-procedure procedure)
    (case procedure
      ('drop drop)
      ('swap swap)
      ('dup dup)
      ('over over)
      ('rot rot)
      ('depth depth)
      ('+ add)
      ('- subtract)
      ('* multiply)
      ('/ divide)
      ('mod mod)
      ('neg neg)
      ('= equal)
      ('> greater)
      ('< less)
      ('not forth-not)
      ('and forth-and)
      ('or forth-or)
      ))

  (define (state words word-counter data-stack call-stack dictionary variables current-action)
    ; Отладка
    ; (write word-counter)
    ; (display " " )
    ; (if (not (equal? (vector-length words) word-counter)) (write (vector-ref words word-counter)))
    ; (write data-stack)
    ; (write call-stack)
    ; (write dictionary)
    ; (write variables)
    ; (write current-action)
    ; (newline)
    (if (not (equal? (vector-length words) word-counter))
        (cond ((and (equal? (vector-ref words word-counter) 'define) ; Начинает словарную статью
                    (not (equal? current-action 'ignore-consequent))
                    (not (equal? current-action 'ignore-alternative)))
               (state words
                      (+ word-counter 2)
                      data-stack
                      call-stack
                      (cons (cons (vector-ref words (+ word-counter 1)) (cons (+ word-counter 2) '())) dictionary)
                      variables
                      'read-definition))
              ((equal? current-action 'read-definition) ; Читает статью, но не выполняет
               (state words
                      (+ word-counter 1)
                      data-stack
                      call-stack
                      dictionary
                      variables
                      (if (equal? (vector-ref words word-counter) 'end)
                          'run-main ; Если дошел до конца статьи, продолжает выполнять программу
                          'read-definition))) ; Иначе продолжает читать
              ((and (or (equal? (vector-ref words word-counter) 'end) ; Если выполнил код в статье, возвращается к месту вызова и продолжает выполнять программу
                        (equal? (vector-ref words word-counter) 'exit))
                    (not (equal? current-action 'ignore-consequent))
                    (not (equal? current-action 'ignore-alternative)))
               (state words
                      (car call-stack)
                      data-stack
                      (cdr call-stack)
                      dictionary
                      variables
                      'run-main))
              ((equal? (vector-ref words word-counter) 'if)
               (state words
                      (+ word-counter 1)
                      (cdr data-stack)
                      call-stack
                      dictionary
                      variables
                      (if (not (equal? (car data-stack) 0))
                          'run-main
                          'ignore-consequent)))
              ((equal? (vector-ref words word-counter) 'else)
               (state words
                      (+ word-counter 1)
                      data-stack
                      call-stack
                      dictionary
                      variables
                      (if (equal? current-action 'ignore-consequent)
                          'run-main
                          'ignore-alternative)))
              ((equal? (vector-ref words word-counter) 'endif)
               (state words
                      (+ word-counter 1)
                      data-stack
                      call-stack
                      dictionary
                      variables
                      'run-main))
              ((or (equal? current-action 'ignore-consequent)
                   (equal? current-action 'ignore-alternative))
               (state words
                      (+ word-counter 1)
                      data-stack
                      call-stack
                      dictionary
                      variables
                      'ignore-consequent))
              ((and (equal? (vector-ref words word-counter) 'set)
                    (not (or (equal? current-action 'ignore-consequent)
                             (equal? current-action 'read-definition))))
               (state words
                      (+ word-counter 2)
                      (drop data-stack)
                      call-stack
                      dictionary
                      (cons (cons (vector-ref words (+ word-counter 1)) (cons (car data-stack) '())) variables)
                      current-action))
              ((and (equal? (vector-ref words word-counter) 'defvar)
                    (not (or (equal? current-action 'ignore-consequent)
                             (equal? current-action 'ignore-alternative)
                             (equal? current-action 'read-definition))))
               (state words
                      (+ word-counter 3)
                      data-stack
                      call-stack
                      dictionary
                      (cons (cons (vector-ref words (+ word-counter 1)) (cons (vector-ref words (+ word-counter 2)) '())) variables)
                      'run-main))
	      ((equal? (vector-ref words word-counter) 'tail)
	       (state words
		      (car (cdr (assoc (vector-ref words (+ word-counter 1)) dictionary)))
		      data-stack
		      call-stack
		      dictionary
		      variables
		      current-action))
              ((assoc (vector-ref words word-counter) variables)
               (state words
                      (+ word-counter 1)
                      (cons (car (cdr (assoc (vector-ref words word-counter) variables))) data-stack)
                      call-stack
                      dictionary
                      variables
                      current-action))
              ((number? (vector-ref words word-counter)) ; Записывает число в стек
               (state words
                      (+ word-counter 1)
                      (cons (vector-ref words word-counter) data-stack)
                      call-stack
                      dictionary
                      variables
                      current-action))
              ((member (vector-ref words word-counter) standard-dictionary) ; Если дали встроенное слово, выполняет операцию над стеком
               (state words
                      (+ word-counter 1)
                      ((standard-procedure (vector-ref words word-counter)) data-stack)
                      call-stack
                      dictionary
                      variables
                      current-action))
              ((assoc (vector-ref words word-counter) dictionary) ; Если дали определнное слово, выполнит код в определении
               (state words
                      (car (cdr (assoc (vector-ref words word-counter) dictionary)))
                      data-stack
                      (cons (+ word-counter 1) call-stack)
                      dictionary
                      variables
                      'run-main))
              )
        data-stack))
  (state program 0 stack '() '() '() 'run-main))

(define the-tests
  (list 
	(test (interpret #() '()) ())
	(test (interpret #(1) '()) (1))
	(test (interpret #(1 2) '()) (2 1))
	(test (interpret #() '(1)) (1))
	(test (interpret #() '(1 2)) (1 2))
	(test (interpret #(1) '(1)) (1 1))
	(test (interpret #(1 2) '(3)) (2 1 3))
	(test (interpret #(3) '(1 2)) (3 1 2))
	(test (interpret #(1 2) '(3 4)) (2 1 3 4))
	(test (interpret #(drop) '(1)) ())
	(test (interpret #(swap) '(1 2)) (2 1))
	(test (interpret #(dup) '(1 2)) (1 1 2))
	(test (interpret #(over) '(1 2)) (2 1 2))
	(test (interpret #(rot) '(1 2 3)) (3 2 1))
	(test (interpret #(depth) '(1 3)) (2 1 3))
	(test (interpret #(+) '(2 1)) (3))
	(test (interpret #(-) '(2 1)) (-1))
	(test (interpret #(*) '(2 3)) (6))
	(test (interpret #(/) '(2 6)) (3))
	(test (interpret #(mod) '(7 9)) (2))
	(test (interpret #(neg) '(1)) (-1))
	(test (interpret #(=) '(2 2)) (-1))
	(test (interpret #(=) '(2 3)) (0))
	(test (interpret #(>) '(1 2)) (-1))
	(test (interpret #(>) '(3 2)) (0))
	(test (interpret #(<) '(2 1)) (-1))
	(test (interpret #(<) '(1 2)) (0))
	(test (interpret #(not) '(0)) (-1))
	(test (interpret #(not) '(2)) (0))
	(test (interpret #(not) '(-2)) (0))
	(test (interpret #(and) '(0 0)) (0))
	(test (interpret #(and) '(1 0)) (0))
	(test (interpret #(and) '(0 1)) (0))
	(test (interpret #(and) '(-1 1)) (-1))
	(test (interpret #(or) '(0 0)) (0))
	(test (interpret #(or) '(1 0)) (-1))
	(test (interpret #(or) '(0 -1)) (-1))
	(test (interpret #(or) '(-1 1)) (-1))
	(test (interpret #(if 1 endif) '(1)) (1))
	(test (interpret #(define -- 1 - end 5 -- --) '()) (3))
	(test (interpret #(define abs dup 0 < if neg endif end 9 abs -9 abs) '()) (9 9))
	(test (interpret #(define =0? dup 0 = end define <0? dup 0 < end define signum =0? if exit endif <0? if drop -1 exit endif drop 1 end 0 signum -5 signum 10 signum) '()) (1 -1 0))
	(test (interpret #(define -- 1 - end define =0? dup 0 = end define =1? dup 1 = end define factorial =0? if drop 1 exit endif =1? if drop 1 exit endif dup -- factorial * end 0 factorial 1 factorial 2 factorial 3 factorial 4 factorial) '()) (24 6 2 1 1))
	(test (interpret #(define =0? dup 0 = end define =1? dup 1 = end define -- 1 - end define fib =0? if drop 0 exit endif =1? if drop 1 exit endif -- dup -- fib swap fib + end define make-fib dup 0 < if drop exit endif dup fib swap -- make-fib end 10 make-fib) '()) (0 1 1 2 3 5 8 13 21 34 55))
	(test (interpret #(define =0? dup 0 = end define gcd =0? if drop exit endif swap over mod gcd end 90 99 gcd 234 8100 gcd) '()) (18 9))
	(test (interpret #(define a 2 dup end a) '(-1 1)) (2 2 -1 1))
	(test (interpret #(1 if define a 3 end endif a) '()) (3))
	(test (interpret #(0 if define a 3 end endif define a 2 end a) '()) (2))
    (test (interpret #(defvar counter 0 define nextnum counter dup 1 + set counter end nextnum nextnum nextnum nextnum + nextnum nextnum *) '()) (20 5 1 0))
   (test (interpret #(1 if 100 else 200 endif) '()) (100))
   (test (interpret #(0 if 100 else 200 endif) '()) (200))
   (test (interpret #(define F 11 22 33 tail G 44 55 end define G 77 88 99 end F) '()) (99 88 77 33 22 11))
   (test (interpret #(define =0? dup 0 = end define gcd =0? if drop exit endif swap over mod tail gcd end 90 99 gcd 234 8100 gcd) '()) (18 9)) 
   ))

(run-tests the-tests)

