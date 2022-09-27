(ns reader
  (:require [clojure.string :as string]))

; [\s,]*
; (~@
;  |[\[\]{}()'`~^@]
;  |"(?:\\.
;     |[^\\"])*"?
;  |;.*
;  |[^\s\[\]{}('"`,;)]*)

(def tokens-regex
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tap [x] (prn "tap> " x) (flush) x)

(defn tokenize [s]
  (->> (re-seq tokens-regex s)
       (map second)
       (remove string/blank?)))

(declare read-form)

(defn read-token* [token]
  (cond
    (re-matches #"-?\d+" token) (Long/parseLong token)
    (re-matches #"-?\d+(\.\d+)?" token) (Double/parseDouble token)
    :else (throw (ex-info "Reader can't read token. Unknown format." {:token token}))))

(defn read-token [[head & remaining]]
  {:result (read-token* head)
   :tokens remaining})

(defn read-list [tokens]
  (loop [tokens (rest tokens) ; pop off open paren
         result []]
    (let [[head & remaining] tokens]
      (cond
        (not= ")" head) (let [form (read-form tokens)]
                          (recur (:tokens form)
                                 (conj result (:result form))))
        (= ")" head) {:result result
                      :tokens remaining}

        ; else unbalanced if close-paren not found
        ))))

(defn read-form [tokens]
  (let [token (first tokens)]
    (cond
      (= "(" token) (read-list tokens)

      ; simplification: in case of loose tokens to evaluate, read the first token only, ignore the rest
      :else (read-token tokens))))

(defn read-form1 [tokens]
  (:result (read-form tokens)))

(defn conj-top [stack form]
  (if-let [parent (peek stack)]
    (conj (pop stack) (conj parent form))
    (list form)))

(defn read-form2
  "Simpler but less fancy.
  Use a explicit stack, instead of the call stack, to track the tree's depth."
  [tokens]
  (prn "tokens: " tokens)
  (peek
   (reduce (fn [stack t]
             (prn "stack: " stack)
             (cond
               (= t "(") (conj stack [])
               (= t ")") (let [form (peek stack) ; pop top stack and merge into parent
                               remaining (pop stack)]
                           (conj-top remaining form))
               :else (let [form (read-token* t)]
                       (conj-top stack form))))
           '()
           tokens)))

(defn read-str-fn
  "Choose which read-form implementation to use"
  [read-form-fn]
  (fn [s]
    (read-form-fn (tokenize s))))

(defn read-str [s]
  ((read-str-fn read-form1) s))

(comment (read-form2 ["-2.1"]))
(comment (read-form1 ["-2.1"]))
(comment (read-form2 ["(" ")"]))
(comment (read-form2 ["(" "1" "2" ")"]))
(comment (read-form2 ["(" "0" "(" "1" "4" ")" "2" "3" ")"]))

(comment (read-str "(0 (1 2) 3 4 (5) 6)"))

(comment (tokenize " ( 123 \"abc\" ) ")
         (tokenize "123"))
