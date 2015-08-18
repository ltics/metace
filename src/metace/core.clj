(ns metace.core
  (:require [metace.cota :refer :all]
            [metace.meta-env :refer :all]
            [metace.meta-eval :refer :all]
            [metace.meta-apply :refer :all]
            [metace.eval-apply :refer :all]))

(defn setup-environment
  []
  (let [initial-env (extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        the-empty-environment)]
    ;;clojure的readmacro读入'true/'false进来就直接是boolean值了 下面这两个基本是摆设
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

(defn parse-file
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [lines (line-seq rdr)
          count (count lines)]
      (doseq [i (range count)]
        (user-print (metaeval (read-string (nth lines i)) the-global-environment))))))

(defn parse-file-multiline
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [lines (filter #(not (or (empty? %)
                                  (.startsWith % ";;")))
                        (line-seq rdr))
          linecount (count lines)]
      (loop [llines lines]
        ;;skip emptyline and commentline
        (if (not (empty? llines))
          (let [line (car llines)]
            (let [left-bracket-num (count (re-seq #"\(" line))
                  right-bracket-num (count (re-seq #"\)" line))]
              ;;如果regex匹配不到re-seq返回的是nil不是空列表
              (if (= left-bracket-num right-bracket-num)
                (do
                  (user-print (metaeval (read-string line) the-global-environment))
                  (recur (cdr llines)))
                (do
                  (let [next-line (cadr llines)]
                    (if next-line
                      (recur (cons (str line next-line) (cddr llines))))))))))))))

(defn -main
  [& args]
  (let [file (get-in (apply hash-map args) ["-f"])]
    (if (nil? file)
      (driver-loop)
      ;;(parse-file file)
      (parse-file-multiline file))))
