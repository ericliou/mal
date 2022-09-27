(ns step1-read-print
  (:require [readline]
            [reader]))

(def read* reader/read-str)
(def eval* identity)
(def print* identity)

(def rep (comp print* eval* read*))

(defn run [& _]
  (while true
    (println (rep (readline/fancy-read-line)))
    (flush)))

