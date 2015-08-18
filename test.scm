;;primitive procedure
(display "===primitive procedure===")
(newline)

(displayln (+ 1 2))
(displayln (- 2 1))
(displayln (* 1 3))
(displayln (/ 3 1))

(define lst '(1 2 3))
(displayln (car lst))
(displayln (cdr lst))
(displayln (cadr lst))
(displayln (caddr lst))

(displayln (cons 3 lst))
(displayln (list 1 2 3))
(displayln (null? '()))
(displayln (null? 3))
(displayln (= 3 3))

(display "===define variable===")
(newline)

(define a 3)
;;this is a symbol
(displayln a)
(set! a 33)
(displayln a)
;;this is a string
(displayln "a")

(display "===define lambda===")
(newline)

;;lambda
(displayln ((lambda (x y) (+ x y)) 1 2))

(define (add2 x y)
    (+ x y))

(displayln (add2 1 2))

(display "===begin block===")
(newline)

(define result
    (begin
        (displayln "add 1 2")
        (+ 1 2)))

(displayln result)

(display "===if===")
(newline)

(define (append x y)
    (if (null? x)
        y
        (cons (car x)
              (append (cdr x) y))))

(displayln (append '(a b c) '(d e f)))

(display "===cond===")
(newline)

(define (abs x)
    (cond ((> x 0) x)
          ((= x 0) (displayln 'zero) 0)
          (else (- x))))

(displayln (abs 3))
(displayln (abs 0))
(displayln (abs -3))

;;compound procedure
(display "===primitive procedure===")
(newline)

(define (foo x y z)
    (list x y z))

(define (bar lst func)
    (func (car lst) (car (cdr lst))))

(displayln (bar (foo 5 6 7) +))
