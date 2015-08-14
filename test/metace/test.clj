(ns metace.test
  (:require [clojure.test :refer :all]
            [metace.cota :refer :all]
            [metace.meta-eval :refer :all]
            [metace.meta-env :refer :all]
            [metace.meta-apply :refer :all]))

(defmacro is= [& body]
  `(is (= ~@body)))

(deftest meta-test
  (testing "quote and read-string"
    (is (= '(lambda (x y) (+ x y)) (read-string "(lambda (x y) (+ x y))")))
    (is (= (cons '(1 2 3) '(4 5 6)) '((1 2 3) 4 5 6)))
    (is (= (car (cons '(1 2 3) '(4 5 6))) '(1 2 3)))
    (is (= (cdr (cons '(1 2 3) '(4 5 6))) '(4 5 6)))))

(deftest cota-test
  (testing "scheme operation in clojure"
    (let [lst '(1 (2 3 4) 5 (6 7))]
      (is (= (car lst) 1))
      (is (= (cdr lst) '((2 3 4) 5 (6 7))))
      (is (= (cadr lst) '(2 3 4)))
      (is (= (cddr lst) '(5 (6 7))))
      (is (= (caadr lst) 2))
      (is (= (caddr lst) 5))
      (is (= (cdadr lst) '(3 4)))
      (is (= (cadddr lst) '(6 7))))))

(deftest eval-test
  (testing "selfeval expression"
    (let [string-exp "3"
          number-exp 3]
      (is (= (self-evaluating? string-exp) true))
      (is (= (self-evaluating? number-exp) true))))
  (testing "variable expression"
    (is (= (variable? 'a) true)))
  (testing "quote expression"
    (let [quote-exp (read-string "(quote a)")]
      (is (= (quoted? quote-exp)))
      (is (= (text-of-quotation quote-exp) 'a))))
  (testing "assignment expression"
    (let [assignment-exp (read-string "(set! a 3)")]
      (is (= (assignment? assignment-exp) true))
      (is (= (assignment-variable assignment-exp) 'a))
      (is (= (assignment-value assignment-exp) 3))))
  (testing "lambda expression"
    (let [lambda-exp (read-string "(lambda (x) (+ x 1))")]
      (is (= (lambda? lambda-exp) true))
      (is (= (lambda-parameters lambda-exp) '(x)))
      (is (= (lambda-body lambda-exp) '(+ x 1)))
      (is (= (make-lambda (lambda-parameters lambda-exp)
                          (lambda-body lambda-exp))
             lambda-exp)))
    (let [lambda-exp (read-string "(lambda (x y) (+ x y))")]
      (is (= (lambda? lambda-exp) true))
      (is (= (lambda-parameters lambda-exp) '(x y)))
      (is (= (lambda-body lambda-exp) '(+ x y)))
      (is (= (make-lambda (lambda-parameters lambda-exp)
                          (lambda-body lambda-exp))
             lambda-exp))))
  (testing "define expression"
    (let [define-exp (read-string "(define (a x y) (+ x y))")]
      (is (= (definition? define-exp) true))
      (is (= (definition-variable define-exp) 'a))
      (is (= (definition-value define-exp) '(lambda (x y) (+ x y)))))
    (let [define-exp (read-string "(define a 3)")]
      (is (= (definition? define-exp) true))
      (is (= (definition-variable define-exp) 'a))
      (is (= (definition-value define-exp) 3))))
  (testing "if expression"
    (let [if-exp (read-string "(if (= a 1) (+ a 1) (+ a 2))")]
      (is (= (if? if-exp) true))
      (is (= (if-predicate if-exp) '(= a 1)))
      (is (= (if-consequent if-exp) '(+ a 1)))
      (is (= (if-alternative if-exp) '(+ a 2)))
      (is (= (make-if (if-predicate if-exp)
                      (if-consequent if-exp)
                      (if-alternative if-exp))
             if-exp)))
    (let [if-exp (read-string "(if (= a 1) (+ a 1))")]
      (is (= (if? if-exp) true))
      (is (= (if-predicate if-exp) '(= a 1)))
      (is (= (if-consequent if-exp) '(+ a 1)))
      (is (= (if-alternative if-exp) 'false))
      (is (= (make-if (if-predicate if-exp)
                      (if-consequent if-exp)
                      (if-alternative if-exp)))
          if-exp)))
  (testing "begin expression"
    (let [begin-exp (read-string "(begin (+ 1 1) (+ 1 2))")]
      (is (= (begin? begin-exp) true))
      (is (= (begin-actions begin-exp) '((+ 1 1) (+ 1 2))))
      (is (= (last-exp? (begin-actions begin-exp)) false))
      (is (= (last-exp? '((+ 1 2))) true))
      (is (= (first-exp (begin-actions begin-exp)) '(+ 1 1)))
      (is (= (rest-exps (begin-actions begin-exp)) '((+ 1 2))))
      (is (= (sequence->exp nil) nil))
      (is (= (sequence->exp '((+ 1 2))) '(+ 1 2)))
      (is (= (sequence->exp (begin-actions begin-exp)) begin-exp))))
  (testing "procedure expression"
    (let [procedure-exp (read-string "(+ (+ 1 2) (+ 1 1))")]
      (is (= (application? procedure-exp) true))
      (is (= (operator procedure-exp) '+))
      (is (= (operands procedure-exp) '((+ 1 2) (+ 1 1))))
      (is (= (no-operands? (operands procedure-exp)) false))
      (is (= (no-operands? (operands '(+))) true))
      (is (= (first-operand (operands procedure-exp)) '(+ 1 2)))
      (is (= (rest-operands (operands procedure-exp)) '((+ 1 1))))))
  (testing "cond expression"
    (let [cond-exp '(cond ((> x 0) x)
                          ((= x 0) (display 'zero) 0)
                          (else (- x)))]
      (is (= (cond? cond-exp) true))
      (is (= (cond-clauses cond-exp) '(((> x 0) x)
                                        ((= x 0) (display 'zero) 0)
                                        (else (- x)))))
      (is (= (cond-predicate (car (cond-clauses cond-exp))) '(> x 0)))
      (is (= (cond-actions (car (cond-clauses cond-exp))) '(x)))
      (is (= (cond-else-clause? (car (cond-clauses cond-exp))) false))
      (is (= (cond-else-clause? (caddr (cond-clauses cond-exp))) true))
      (is (= (cond->if cond-exp) '(if (> x 0)
                                    x
                                    (if (= x 0)
                                      (begin (display 'zero)
                                             0)
                                      (- x))))))))

(deftest env-test
  (testing "frame operations"
    (let [vars '(a b c)
          vals '(1 2 3)
          frame (make-frame vars vals)]
      (is (= (frame-variables frame) vars))
      (is (= (frame-values frame) vals))))
  (testing "env operations"
    (let [init-env (extend-environment (primitive-procedure-names)
                                       (primitive-procedure-objects)
                                       the-empty-environment)]
      ;;用cadr是为了要去掉'primitive
      (is (= (apply (cadr (lookup-variable-value '+ init-env)) '(1 2 3)) 6))
      (do
        (add-binding-to-frame! 'mod (list 'primitive mod) (first-frame init-env))
        (is (= (apply (cadr (lookup-variable-value 'mod init-env)) '(3 6)) 3)))
      (do
        (define-variable! 'add (list 'primitive +) init-env)
        (is= (apply (cadr (lookup-variable-value 'add init-env)) '(1 2 3)) 6))
      (do
        ;;所以在往环境里放入新的operation的时候要用(list 'primitive xxx)而不是'(primitive xxx)因为后者会把真实操作的函数也quote成了一个symbol
        (set-variable-value! 'add '(primitive +) init-env)
        (is= (cadr (lookup-variable-value 'add init-env)) '+)))))

(deftest apply-test
  (testing "compound procedure"
    (let [init-env (extend-environment (primitive-procedure-names)
                                       (primitive-procedure-objects)
                                       the-empty-environment)
          compound-procedure (make-procedure '(x y) '(+ x y) init-env)]
      (is= compound-procedure (list 'procedure '(x y) '(+ x y) init-env))
      (is= (compound-procedure? compound-procedure) true)
      (is= (procedure-parameters compound-procedure) '(x y))
      (is= (procedure-body compound-procedure) '(+ x y))
      (is= (procedure-environment compound-procedure) init-env))))