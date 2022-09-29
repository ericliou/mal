(ns printer
  (:require [clojure.string :as string]))

(defn abs->str [abs]
    (cond
      (symbol? abs) (str abs)
      (number? abs) (str abs)
      (vector? abs) (str "(" (string/join " " (map abs->str abs)) ")")
      ))

(comment
  (abs->str ['+ ['- 1 2] 1 2 3])
  )

