(ns step4-if-fn-do
  (:require [readline]
            [reader]
            [printer]))

(def repl-env
  {'+ +
   '- -
   '* *
   '/ /
   'do (fn [& exprs] (last exprs))})

(declare eval*)

(defn eval-asts [{:keys [env ast] :as state}]
  (let [r (reductions (fn [acc x] (eval* {:env (:env acc) :ast x}))
                      state
                      ast)]
    {:env (:env (last r))
     :evaluation (map :evaluation (rest r))}))

(defn eval-ast [{:keys [env ast] :as state}]
  (cond
    (symbol? ast) (if-let [sym-val (get env ast)]
                    (assoc state :evaluation sym-val)
                    (throw (ex-info (str "Symbol '" ast "' not found.")
                                    {:error ::unresolved-symbol
                                     :symbol ast})))
    (list? ast) (eval-asts state)
    (vector? ast) (update (eval-asts state) :evaluation #(into [] %))
    (map? ast) (update (eval-asts state) :evaluation #(into {} %))
    :else (assoc state :evaluation ast)))

(defn eval-function [{:keys [env ast] :as state}]
  (cond
    (= (first ast) 'let*)
    (let [[_let* bindings body] ast
          let-env (reduce (fn [env [sym-name expr]]
                            (assoc env sym-name (eval* {:env env :ast expr})))
                          env
                          (partition 2 bindings))]
      (eval* {:env let-env :ast body}))

    (= (first ast) 'def!)
    (let [[_def! sym-name form] ast
          {:keys [evaluation]} (eval* {:env env :ast form})]
      {:env (assoc env sym-name evaluation)
       :evaluation evaluation})

    (= (first ast) 'if)
    (let [[_if test-expr then-expr else-expr] ast
          {:keys [env evaluation]} (eval* {:env env :ast test-expr})]
      (eval* {:env env :ast (if evaluation
                              then-expr
                              else-expr)}))

    ;; non-special forms
    :else (update (eval-ast state) :evaluation #(apply (first %) (rest %)))))

(defn eval* [{:keys [env ast] :as state}]
  (cond
    (and (list? ast) (empty? ast))  {:env env :ast ast}
    (list? ast) (eval-function {:env env :ast ast})
    :else  (eval-ast {:env env :ast ast})))

(def read* reader/read-str)

(defn print* [state]
  (update state :evaluation printer/ast->string))

(defn rep [env s]
  (print* (eval* {:env env :ast (read* s)})))

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

