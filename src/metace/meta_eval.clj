(ns metace.meta-eval
  (:require [metace.cota :refer :all]))

(def car first)
(def cadr #(first (rest %)))
(def cddr #(rest (rest %)))
(def caadr #(first (first (rest %))))
(def caddr #(first (rest (rest %))))
(def cdadr #(rest (cadr %)))
(def cadddr #(first (rest (cddr %))))

(defn self-evaluating?
  [exp]
  (cond
    (number? exp) true
    (string? exp) true
    :else false))

(def variable? symbol?)

(defn tagged-list?
  [exp tag]
  (if (list? exp)
    (= (car exp) tag)
    false))

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

(comment
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
      (let? exp) (metaeval (let->lambda exp) env)
      :else (error "Unknown expression type -- EVAL" exp))))
