(ns metace.eval-apply
  (:require [metace.cota :refer :all]
            [metace.meta-env :refer :all]
            [metace.meta-eval :refer :all]
            [metace.meta-apply :refer :all]))

(declare list-of-values
         eval-if
         eval-sequence
         eval-assignment
         eval-definition
         metaapply)

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
    (application? exp) (metaapply (metaeval (operator exp) env)
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

(defn metaapply
  [procedure arguments]
  ;;基础过程和复合过程
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence (procedure-body procedure)
                                                       (extend-environment
                                                         (procedure-parameters procedure)
                                                         arguments
                                                         (procedure-environment procedure)))
        :else (error "Unknown procedure type -- APPLY" procedure)))
