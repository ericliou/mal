(ns step0-repl
  (:require [readline]))

(def read* identity)
(def eval* identity)
(def print* identity)

(def rep (comp print* eval* read*))

(defn run [& _]
  (while true
    (println (rep (readline/fancy-read-line)))
    (flush)))
