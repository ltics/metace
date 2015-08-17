(define a 3)
a
"a"
((lambda (x y) (+ x y)) 1 2)

;;compound procedure
(define (foo x y z)
   (list x y z))

(define (bar lst func)
   (func (car lst) (car (cdr lst))))

(bar (foo 5 6 7) +)
