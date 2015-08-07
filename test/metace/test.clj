(ns metace.test
  (:require [clojure.test :refer :all]
            [metace.meta-eval :refer :all]))

(deftest meta-test
  (testing "quote and read-string"
    (is (= '(lambda (x y) (+ x y)) (read-string "(lambda (x y) (+ x y))")))))

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
      (is (= (if-alternative if-exp) '(+ a 2))))
    (let [if-exp (read-string "(if (= a 1) (+ a 1))")]
      (is (= (if? if-exp) true))
      (is (= (if-predicate if-exp) '(= a 1)))
      (is (= (if-consequent if-exp) '(+ a 1)))
      (is (= (if-alternative if-exp) 'false)))))
