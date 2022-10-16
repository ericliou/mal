(ns step3-env
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
                    (throw (ex-info (str "Symbol '" ast "' not found.")
                                    {:error ::unresolved-symbol
                                     :symbol ast})))
    ;; TODO: can previous evals in a list affect the next ones? `(do (def a 1) (def b (inc a)))`
    (list? ast) (doall (map #(eval* env %) ast))
    (vector? ast) (mapv #(eval* env %) ast)
    (map? ast) (into {} (map (fn [[k v]]
                               (vector k (eval* env v))) ast))
    :else ast))

(defn eval-function [env ast]
  (cond
    (= (first ast) 'let*)
    (let [[_let* bindings body] ast
          let-env (reduce (fn [env [sym-name expr]]
                            (assoc env sym-name (eval* env expr)))
                          env
                          (partition 2 bindings))]
      (eval* let-env body))

    :else
    (let [[f & args] (eval-ast env ast)]
      (apply f args))))

(defn- changes-env? [ast]
  (and (list? ast) (= 'def! (first ast))))

(defn eval* [env ast]
  (cond
    (and (list? ast) (empty? ast)) ast
    (list? ast) (eval-function env ast)
    :else (eval-ast env ast)))

(defn- eval-and-update-env [env ast]
  (cond (= (first ast) 'def!)
        (let [[_def! sym-name form] ast
              result (eval* env form)]
          {:env (assoc env sym-name result)
           :evaluation result})))

(defn root-eval*
  "Note: only considering root level `def!` for now."
  [env ast]
  (if (changes-env? ast)
    (eval-and-update-env env ast)
    {:evaluation (eval* env ast) :env env}))

(def read* reader/read-str)

(defn print* [state]
  (update state :evaluation printer/ast->string))

(defn rep [env s]
  (print* (root-eval* env (read* s))))

(defn try-rep [env s]
  (try
    (rep env s)
    (catch Exception ex {:evaluation (str ex) :env env :ex ex})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [& _]
  (loop [env repl-env]
    (let [{:keys [env evaluation]} (try-rep env (readline/fancy-read-line))]
      (println evaluation) (flush)
      (recur env))))

