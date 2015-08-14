(ns metace.meta-env
  (:require [metace.cota :refer :all]))

;;外层环境
(defn enclosing-environment
  [env]
  (cdr env))

(defn first-frame
  [env]
  (car env))

(def the-empty-environment '())

(defn make-frame
  [variables values]
  (atom (cons variables values)))

(defn frame-variables
  [frame]
  (car @frame))

(defn frame-values
  [frame]
  (cdr @frame))

(defn add-binding-to-frame!
  [var val frame]
  (reset! frame
    (cons (cons var (frame-variables frame))
          (cons (atom val) (frame-values frame)))))

;;扩展环境类似于压栈出栈 最后被放入环境框架中的vars vals会被最先被遍历检查 因为这一部分的环境在较深的代码block中
(defn extend-environment
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error "Too many arguments supplied -> " vars vals)
      (error "Too few arguments supplied -> " vars vals))))

;;从环境框架中寻找某一个变量的值
(defn lookup-variable-value
  [var env]
  (defn env-loop [env]
    (defn scan [vars vals]
      (cond (empty? vars) (env-loop (enclosing-environment env))
            (= var (car vars)) @(car vals)
            :else (scan (cdr vars) (cdr vals))))
    (if (= env the-empty-environment)
      (error "Unbound variable -> " var)
      (let [frame (first-frame env)]
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;;在环境框架中改变一个变量的值
(defn set-variable-value!
  [var val env]
  (defn env-loop [env]
    (defn scan [vars vals]
      (cond (empty? vars) (env-loop (enclosing-environment env))
            (= var (car vars)) (reset! (car vals) val)
            :else (scan (cdr vars) (cdr vals))))
    (if (= env the-empty-environment)
      (error "Unbound variable -- SET! -> " var)
      (let [frame (first-frame env)]
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

;;在第一个环境框架中设置或者改变一个变量的值
(defn define-variable!
  [var val env]
  (let [frame (first-frame env)]
    (defn scan [vars vals]
      (cond (empty? vars) (add-binding-to-frame! var val frame)
            (= var (car vars)) (reset! (car vals) val)
            :else (scan (cdr vars) (cdr vals))))
    (scan (frame-variables frame)
          (frame-values frame))))
