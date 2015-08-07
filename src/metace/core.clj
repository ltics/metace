(ns metace.core
  (:require [metace.meta-env :refer :all]
            [metace.meta-eval :refer :all]
            [metace.meta-apply :refer :all]
            [metace.eval-apply :refer :all]))

(defn setup-environment
  []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment (setup-environment))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(defn prompt-for-input
  [string]
  (do (prn) (prn) (prn string) (prn)))

(defn announce-output
  [string]
  (do (prn) (prn string) (prn)))

(defn user-print
  [object]
  (if (compound-procedure? object)
    (prn (list 'compound-procedure
               (procedure-parameters object)
               (procedure-body object)
               '<procedure-env>))
    (prn object)))

(defn driver-loop
  []
  (do
    (prompt-for-input input-prompt)
    (let [input (read)]
      (let [output (metaeval input the-global-environment)]
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop)))

(defn -main
  [& args]
  (driver-loop))
