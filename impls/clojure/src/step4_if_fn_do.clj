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


;; ------------------------------------------
;; TODO move to env ns
;; root env

(def root-env (atom repl-env))

(defn get-sym [env ast]
  (or (get env ast)
      (get @root-env ast)
      (throw (ex-info (str "Symbol '" ast "' not found.")
                      {:error ::unresolved-symbol
                       :symbol ast}))))

(defn put-root-sym [sym value]
  (swap! root-env assoc sym value))

;; ------------------------------------------

(declare eval*)

(defn eval-ast
  "Evaluate the ast and preserve the data structure.
  e.g. if the ast is a vector, it represent a vector, so it should return a vector."
  [env ast]
  (cond
    (symbol? ast) (get-sym env ast)
    (list? ast) (doall (map #(eval* env %) ast))
    (vector? ast) (mapv #(eval* env %) ast)
    (map? ast) (into {} (map (fn [[k v]]
                               (vector k (eval* env v))) ast))
    :else ast))

(defn eval-function
  "Can do side effects."
  [env ast]
  (cond
    (= (first ast) 'let*)
    (let [[_let* bindings body] ast
          let-env (reduce (fn [env [sym-name expr]]
                            (assoc env sym-name (eval* env expr)))
                          env
                          (partition 2 bindings))]
      (eval* let-env body))

    (= (first ast) 'def!)
    (let [[_def! sym-name form] ast
          evaluation (eval* env form)]
      (put-root-sym sym-name evaluation)
      evaluation)

    (= (first ast) 'if)
    (let [[_if test-expr then-expr else-expr] ast
          evaluation (eval* env test-expr)]
      (eval* env (if evaluation
                   then-expr
                   else-expr)))

    (= (first ast) 'fn*)
    (let [[_fn* args body] ast]
      (fn [& args*]
        ;; Note: it seems that clojure compiles and resolves symbols at this stage. This doesn't.
        (eval* (reduce (fn [env [arg value]]
                         (assoc env arg value))
                       env ; this implements lexical scope
                       (partition 2 (interleave args args*)))
               body)))

    ;; non-special forms
    :else (let [[f & args] (eval-ast env ast)]
            (apply f args))))

(defn eval* [env ast]
  (cond
    (and (list? ast) (empty? ast)) ast
    (list? ast) (eval-function env ast)
    :else (eval-ast env ast)))

(def read* reader/read-str)

(def print* printer/ast->string)

(defn rep [s]
  ;; initialize env with empty map, root env is globally accessed
  (print* (eval* {} (read* s))))

(defn try-rep [s]
  (try
    (rep s)
    (catch Exception ex {:exception ex})))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [& _]
  (while true
    (println (try-rep (readline/fancy-read-line)))
    (flush)))
