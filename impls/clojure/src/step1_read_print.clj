(ns step1-read-print
  (:require [readline]
            [reader]
            [printer]))

(def read* reader/read-str)
(def eval* identity)
(def print* printer/abs->string)

(def rep (comp print* eval* read*))

(defn run [& _]
  (while true
    (println (rep (readline/fancy-read-line)))
    (flush)))

