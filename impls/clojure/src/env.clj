(ns env
  (:require [core]))

(defn get-sym [env sym]
  (if-let [env (some-> env deref)]
    (or (get env sym)
        (get-sym (::parent env) sym))
    (throw (ex-info (str "Symbol '" sym "' not found.")
                    {:error ::unresolved-symbol
                     :symbol sym}))))

(defn put-sym! [env sym val]
  (swap! env assoc sym val)
  env)

(defn new-env
  "Using a mutable env makes easier to impl dynamic scope and recursive
  functions that reference itself."
  [parent-env]
  (atom {::parent parent-env}))

(def ^:private root-env (atom {}))

(defn init-root-env! []
  (reset! root-env core/namespace)
  root-env)

(defn put-root-sym! [env sym value]
  (swap! root-env assoc sym value)
  root-env
  #_(if-let [parent-env (some-> env deref ::parent)]
    (put-root-sym! parent-env sym value)
    (do (swap! env assoc sym value)
        env)))
