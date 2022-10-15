(ns printer
  (:require [clojure.string :as string]))

(defn- from-string
  "Reverse what read-str does with strings.
  - explicitly print \n
  - escape double quote
  - escape backslash
  - surround with double quotes"
  [abs]
  (-> abs
      (string/replace "\\" "\\\\")
      (string/replace "\n" "\\n")
      (string/replace "\"" "\\\"")
      (#(str "\"" % "\""))))

(comment (print (from-string "hi\nhi\"ho")))

(declare ast->string)

(defn coll-elements->string [coll]
  (string/join " " (map ast->string coll)))

(defn ast->string
  "The main fn is not using clojure.core/pr on purpose,
  so we implement string conversion ourselves."
  [ast]
    (cond
      (symbol? ast) (str ast)
      (number? ast) (str ast)
      (nil? ast) "nil"
      (list? ast) (str "(" (coll-elements->string ast) ")")
      (vector? ast) (str "[" (coll-elements->string ast) "]")
      (map? ast) (str "{" (coll-elements->string (apply concat ast)) "}")
      (string? ast) (from-string ast)
      :else (str ast)))

(comment
  (ast->string ['+ ['- 1 2] 1 2 3])
  )

