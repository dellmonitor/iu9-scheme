(define-syntax when
  (syntax-rules ()
    ((when cond? . expr)
     (if cond?
       (and (begin . expr))))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? . expr)
     (if (not cond?)
       (and (begin . expr))))))

(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . expr)
     (let loop ((vals xs))
       (if (not (null? vals))
           (let ((x (car vals)))
             (begin . expr)
             (loop (cdr vals))))))
    ((for xs as x . expr)
     (for x in xs . expr))))

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))
(newline)

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))
(newline)

(define-syntax while
  (syntax-rules ()
    ((while cond? . expr)
     (let loop ()
       (if cond?
	 (begin (begin . expr) (loop)))))))

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr ...) until cond?)
     (let loop ()
       (begin expr ...
              (if (not cond?) (loop)))))))

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))

(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << operand)
     (display operand))
    ((count << endl rest ...)
     (begin (newline)
	    (cout rest ...)))
    ((count << operand rest ...)
     (begin (display operand)
	    (cout rest ...)))))

(cout << "a = " << 1 << endl << "b = " << 2 << endl)
