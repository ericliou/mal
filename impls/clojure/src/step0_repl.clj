(ns step0-repl)

(def read* identity)
(def eval* identity)
(def print* identity)

(def rep (comp print* eval* read*))

(defn run [_]
  (while true
    (print "user> ")
    (flush)
    (println (rep (read-line)))
    (flush)))
