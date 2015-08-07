(ns metace.cota)

(def car first)
(def cdr rest)
(def cadr #(first (cdr %)))
(def cddr #(cdr (cdr %)))
(def caadr #(first (first (cdr %))))
(def caddr #(first (cdr (cdr %))))
(def cdadr #(cdr (cadr %)))
(def cadddr #(first (cdr (cddr %))))

(defn tagged-list?
  [exp tag]
  (if (list? exp)
    (= (car exp) tag)
    false))

(defn error [& msg] (prn (apply str msg)))