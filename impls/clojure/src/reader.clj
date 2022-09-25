(ns reader
  (:require
   clojure.edn
   [clojure.string :as string]))

; [\s,]*
; (~@
;  |[\[\]{}()'`~^@]
;  |"(?:\\.
;     |[^\\"])*"?
;  |;.*
;  |[^\s\[\]{}('"`,;)]*)

(def tokens-regex
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")

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

(defn read-form2
  "Simpler but less fancy.
  Use a explicit stack, instead of the call stack, to track the tree depth."
  [tokens]
  (reduce (fn [stack t]
            (cond
              (= t "(") (conj stack [])
              (= t ")") (let [current (peek stack) ; pop top stack and merge into parent
                              remaining (pop stack)
                              parent (peek remaining)
                              new-parent (conj parent current)]
                          (conj (pop remaining) new-parent))
              :else (let [token (read-token* t)]
                      (if (peek stack)
                        (conj (pop stack) (conj (peek stack) token)) ; update top stack with new token

                        ; simplification: in case of loose tokens to evaluate, read the first token only, ignore the rest
                        (conj stack token)))))
          tokens))


(comment (read-form2 ["-2.1"])
         ; => -2.1
         )
(comment (read-form ["-2.1"])
         ; => -2.1
         )
(comment (read-form ["(" ")"])
         ; => ( 1 2 )
         )
(comment (read-form2 ["(" "1" "2" ")"])
         ; => ( 1 2 )
         )
(comment (read-form2 ["(" "0" "(" "1" "4" ")" "2" "3" ")"])
         ; => ( 1 2 )
         )

(defn read-str [s]
  (let [tokens (tokenize s)]
    (read-form tokens)))

(comment (read-str "(0 (1 2) 3 4 (5) 6)"))

(comment (tokenize " ( 123 \"abc\" ) ")
         (tokenize "123"))

