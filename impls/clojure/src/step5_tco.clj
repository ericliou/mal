(ns step5-tco
  (:require [readline]
            [reader]
            [printer]
            [core]
            [env]))

(declare eval*)

(defn- map-fn-bindings [bindings args]
  (loop [result {}
         bindings bindings
         args args]
    (cond
      (empty? bindings) result

      (= '& (first bindings))
      (assoc result (second bindings) (apply list args))

      :else (recur (assoc result (first bindings) (first args))
                   (rest bindings)
                   (rest args)))))

(defn eval-ast
  "Evaluate the ast and preserve the data structure.
  e.g. if the ast is a vector, it represent a vector, so it should return a vector."
  [env ast]
  (cond
    (symbol? ast) (env/get-sym env ast)
    (list? ast) (doall (map #(eval* env %) ast))
    (vector? ast) (mapv #(eval* env %) ast)
    (map? ast) (into {} (map (fn [[k v]]
                               (vector k (eval* env v))) ast))
    :else ast))

(defn eval* [env ast]
  (loop [env env
         ast ast]
    (cond
      (and (list? ast) (empty? ast)) ast

      (list? ast)
      (cond
        (= (first ast) 'let*)
        (let [[_let* bindings body] ast
              let-env (reduce (fn [env [sym-name expr]]
                                (env/put-sym! env sym-name (eval* env expr)))
                              (env/new-env env)
                              (partition 2 bindings))]
          (recur let-env body))

        (= (first ast) 'def!)
        (let [[_def! sym-name form] ast
              evaluation (eval* env form)]
          (env/put-root-sym! env sym-name evaluation)
          evaluation)

        (= (first ast) 'if)
        (let [[_if test-expr then-expr else-expr] ast
              evaluation (eval* env test-expr)]
          (recur env (if evaluation
                       then-expr
                       else-expr)))

        (= (first ast) 'fn*)
        (let [[_fn* bindings body] ast]
          {:ast body
           :params bindings
           :env env
           ;; Note that :fn is required on step 9
           ;; for the map and apply core functions).
           :fn (fn [& args]
             (eval* (reduce (fn [env [arg value]]
                              (env/put-sym! env arg value))
                            (env/new-env env)
                            (map-fn-bindings bindings args))
                    body))})

        ;; non-special forms
        :else (let [[f & args] (eval-ast env ast)]
                (if (:fn f)
                  (recur (reduce (fn [env [arg value]]
                              (env/put-sym! env arg value))
                            (env/new-env env)
                            (map-fn-bindings (:params f) args))
                         (:ast f))
                  (apply f args))))
      :else (eval-ast env ast))))

(def read* reader/read-str)

(def print* printer/ast->string)

(defn rep [env s]
  (print* (eval* env (read* s))))

(defn try-rep [env s]
  (try
    (rep env s)
    (catch Exception ex {:exception ex})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [& _]
  (let [root-env (env/init-root-env!)]
    (try-rep root-env "(def! not (fn* (a) (if a false true)))")
    (while true
      (println (try-rep root-env (readline/fancy-read-line)))
      (flush))))
