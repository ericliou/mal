(ns step4-if-fn-do
  (:require [readline]
            [reader]
            [printer]))

(def repl-env
  [{'+ +
    '- -
    '* *
    '/ /
    'do (fn [& exprs] (last exprs))}])

;; ------------------------------------------
;; move to env ns

(defn get-sym [env ast]
  (if (peek env)
    (or (get (peek env) ast) (get-sym (pop env) ast))
    (throw (ex-info (str "Symbol '" ast "' not found.")
                    {:error ::unresolved-symbol
                     :symbol ast}))))

(defn put-sym [env sym value]
  (conj (pop env) (assoc (peek env) sym value)))

(defn put-root-sym [env sym value]
  (update env 0 assoc sym value))

(defn new-scope [env]
  (conj env {}))

(defn pop-scope [env]
  (pop env))
;; ------------------------------------------

(declare eval*)

(defn eval-asts [{:keys [env ast] :as state}]
  (let [r (reductions (fn [acc x] (eval* {:env (:env acc) :ast x}))
                      state
                      ast)]
    {:env (:env (last r))
     :evaluation (map :evaluation (rest r))}))

(defn eval-ast [{:keys [env ast] :as state}]
  (cond
    (symbol? ast) (assoc state :evaluation (get-sym env ast))
    (list? ast) (eval-asts state)
    (vector? ast) (update (eval-asts state) :evaluation #(into [] %))
    (map? ast) (update (eval-asts state) :evaluation #(into {} %))
    :else (assoc state :evaluation ast)))

(defn eval-function [{:keys [env ast] :as state}]
  (cond
    (= (first ast) 'let*)
    (let [[_let* bindings body] ast
          let-env (reduce (fn [env [sym-name expr]]
                            (let [{:keys [env evaluation]} (eval* {:env env :ast expr})]
                              (put-sym env sym-name evaluation)))
                          (new-scope env)
                          (partition 2 bindings))]
      (update (eval* {:env let-env :ast body}) :env pop-scope))

    (= (first ast) 'def!)
    (let [[_def! sym-name form] ast
          {:keys [evaluation]} (eval* {:env env :ast form})]
      {:env (put-root-sym env sym-name evaluation)
       :evaluation evaluation})

    (= (first ast) 'if)
    (let [[_if test-expr then-expr else-expr] ast
          {:keys [env evaluation]} (eval* {:env env :ast test-expr})]
      (eval* {:env env :ast (if evaluation
                              then-expr
                              else-expr)}))

    (= (first ast) 'fn*)
    (let [[_fn* args body] ast]
      (assoc state
             :evaluation
             (fn [& args*]
               ;; TODO needs to remove the bindings. Add tests.
               ;; Note: it seems that clojure compiles and resolves symbols at this stage. This doesn't.
               ;; TODO deal with exceptions, env stack will be wrong
               (-> (eval* {:env (reduce (fn [env [sym value]]
                                          (put-sym env sym value))
                                        (new-scope env)
                                        (partition 2 (interleave args args*)))
                           :ast body})
                   (update :env pop-scope)))))

    ;; non-special forms
    ;; BUG fn* will be called here, it will return full :env and :evaluation map.
    ;; But other fns like '+ only returns the value
    :else (tap (update (tap (eval-ast state)) :evaluation #(apply (first %) (rest %))))))

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

