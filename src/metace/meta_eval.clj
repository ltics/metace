(ns metace.meta-eval
  (:require [metace.cota :refer :all]
            [metace.meta-apply :refer :all]))

(defn self-evaluating?
  [exp]
  (cond
    (number? exp) true
    (string? exp) true
    :else false))

(def variable? symbol?)

(defn quoted?
  [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation
  [exp]
  (cadr exp))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable
  [exp]
  (cadr exp))

(defn assignment-value
  [exp]
  (caddr exp))

(defn lambda?
  [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters
  [exp]
  (cadr exp))

(defn lambda-body
  [exp]
  (caddr exp))

(defn make-lambda
  [parameters body]
  (list 'lambda parameters body))

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn definition-variable
  [exp]
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(defn definition-value
  [exp]
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (caddr exp))))

(defn if?
  [exp]
  (tagged-list? exp 'if))

(defn if-predicate
  [exp]
  (cadr exp))

(defn if-consequent
  [exp]
  (caddr exp))

(defn if-alternative
  [exp]
  (if (not (nil? (cadddr exp)))
    (cadddr exp)
    'false))

(defn make-if
  [predicate consequent alternative]
  (if (= alternative 'false)
    (list 'if predicate consequent)
    (list 'if predicate consequent alternative)))

(defn begin?
  [exp]
  (tagged-list? exp 'begin))

(defn begin-actions
  [exp]
  (cdr exp))

(defn last-exp?
  [seq]
  (empty? (cdr seq)))

(defn first-exp
  [seq]
  (car seq))

(defn rest-exps
  [seq]
  (cdr seq))

(defn make-begin
  [seq]
  (cons 'begin seq))

(defn sequence->exp
  [seq]
  (cond
    (nil? seq) seq
    (last-exp? seq) (first-exp seq)
    :else (make-begin seq)))

(defn application?
  [exp]
  (list? exp))

(defn operator
  [exp]
  (car exp))

(defn operands
  [exp]
  (cdr exp))

;;其实只用empty?也够了
(defn no-operands?
  [ops]
  (or (empty? ops) (nil? ops)))

(defn first-operand
  [ops]
  (car ops))

(defn rest-operands
  [ops]
  (cdr ops))

;;派生表达式 其实已经有点宏的意思了 也就用一些基本的元素组成新的元素 但是在编译期展开成基本元素 也就是在运行时执行的还是基本元素
(defn cond?
  [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses
  [exp]
  (cdr exp))

(defn cond-predicate
  [clause]
  (car clause))

(defn cond-actions
  [clause]
  (cdr clause))

(defn cond-else-clause?
  [clause]
  (= (cond-predicate clause) 'else))

(defn expand-clauses
  [clauses]
  (if (nil? clauses)
    'false
    (let [first-part (car clauses)
          rest-part (cdr clauses)]
      (if (cond-else-clause? first-part)
        ;;其实只要empty?就好了
        (if (or (empty? rest-part) (nil? rest-part))
          (sequence->exp (cond-actions first-part))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first-part)
                 (sequence->exp (cond-actions first-part))
                 (expand-clauses rest-part))))))

(defn cond->if
  [exp]
  (expand-clauses (cond-clauses exp)))

(declare list-of-values
         eval-if
         eval-sequence
         eval-assignment
         eval-definition
         set-variable-value!
         define-variable!
         lookup-variable-value)

(defn metaeval
  [exp env]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (begin? exp) (eval-sequence (begin-actions exp) env)
    (cond? exp) (metaeval (cond->if exp) env)
    (application? exp) (apply (metaeval (operator exp) env)
                              (list-of-values (operands exp) env))
    :else (error "Unknown expression type -- EVAL" exp)))

(defn list-of-values
  [ops env]
  (if (no-operands? ops)
    '()
    (cons (metaeval (first-operand ops) env)
          (list-of-values (rest-operands ops) env))))

(defn eval-if
  [exp env]
  (if (true? (metaeval (if-predicate exp) env))
    (metaeval (if-consequent exp) env)
    (metaeval (if-alternative exp) env)))

(defn eval-sequence
  [exps env]
  (cond (last-exp? exps) (metaeval (first-exp exps) env)
        :else (do (metaeval (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))

(defn eval-assignment
  [exp env]
  (set-variable-value! (assignment-variable exp)
                       (metaeval (assignment-value exp) env)
                       env) 'ok)

(defn eval-definition
  [exp env]
  (define-variable! (definition-variable exp)
                    (metaeval (definition-value exp) env)
                    env) 'ok)
