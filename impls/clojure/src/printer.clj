(ns printer
  (:require [clojure.string :as string]))

(defn- from-string
  "Reverse what read-str does with strings.
  - explicitly print \n
  - escape double quote
  - surround with double quotes"
  [abs]
  (-> abs
      (string/replace "\n" "\\n")
      (string/replace "\"" "\\\"")
      (#(str "\"" % "\""))))

(comment (print (from-string "hi\nhi\"ho")))

(defn abs->string
  "The main fn is not using clojure.core/pr on purpose,
  so we implement string conversion ourselves."
  [abs]
    (cond
      (symbol? abs) (str abs)
      (number? abs) (str abs)
      (nil? abs) "nil"
      (vector? abs) (str "(" (string/join " " (map abs->string abs)) ")")
      (string? abs) (from-string abs)
      :else (str abs)))

(comment
  (abs->string ['+ ['- 1 2] 1 2 3])
  )

