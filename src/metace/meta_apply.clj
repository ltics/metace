(ns metace.meta-apply
  (:require [metace.cota :refer :all]))

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

(defn metaapply
  [procedure arguments global-env])
