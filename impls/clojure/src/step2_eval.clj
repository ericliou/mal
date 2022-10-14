(ns step2-eval
  (:require [readline]
            [reader]
            [printer]))

(def repl-env
  {'+ +
   '- -
   '* *
   '/ /})

(declare eval*)

(defn eval-ast [env ast]
  (cond
    (symbol? ast) (if-let [sym (get env ast)]
                    sym
                    (throw (ex-info "Unresolved symbol." {:error ::unresolved-symbol
                                                          :symbol ast})))
    (list? ast) (map #(eval* env %) ast)
    :else ast))

(defn eval* [env ast]
  (cond
    (and (list? ast) (empty? ast)) ast
    (list? ast) (let [[f & args] (eval-ast env ast)]
                  (apply f args))
    :else (eval-ast env ast)))

(def read* reader/read-str)
(def print* printer/abs->string)

(def rep (comp print* (partial eval* repl-env) read*))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [& _]
  (while true
    (try
      (println (rep (readline/fancy-read-line)))
      (catch Exception ex
        (println ex)))
    (flush)))

