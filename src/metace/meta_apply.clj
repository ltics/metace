(ns metace.meta-apply
  (:require [metace.cota :refer :all]))

;;compound procedure
(defn make-procedure
  [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure?
  [p]
  (tagged-list? p 'procedure))

(defn procedure-parameters
  [p]
  (cadr p))

(defn procedure-body
  [p]
  (caddr p))

(defn procedure-environment
  [p]
  (cadddr p))

;;primitive procedure
(def primitive-procedures
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '= =)))

(defn primitive-procedure?
  [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation
  [proc]
  (cadr proc))

(defn primitive-procedure-names
  []
  (map car primitive-procedures))

;;包在atom中做mutable
(defn primitive-procedure-objects
  []
  (map (fn [proc]
         (atom (list 'primitive (cadr proc))))
       primitive-procedures))

(defn apply-primitive-procedure
  [proc args]
  (apply (primitive-implementation proc) args))
