(ns reader
  (:require [clojure.string :as string]))

; [\s,]*                 <- spaces
; (~@                    <- macro
;  |[\[\]{}()'`~^@]      <- special chars []{}()'`^@
;  |"(?:\\.|[^\\"])*"?   <- string
;  |;.*                  <- comments
;  |[^\s\[\]{}('"`,;)]*) <- symbols, numbers, bool, nil

(def tokens-regex
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)")

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tap [x] (prn "tap> " x) (flush) x)

(defn tokenize [s]
  (->> (re-seq tokens-regex s)
       (map second)
       (remove string/blank?)))

(declare read-form*)

(defn second-last [s]
  (second (reverse s)))

(defn- validate-string-atom
  "Expects a string that at least starts and ends with double-quotes."
  [a]
  (let [sanitized (string/replace a "\\\\" " ")] ; remove \\ to find out if last double-quote has been escaped
    (when (or (= (second-last sanitized) \\)
              (not= \" (first sanitized) (last sanitized))
              (< (count sanitized) 2))
      (throw (ex-info "EOF. Unbalanced quotes" {:error ::invalid-string :atom sanitized}))))
  a)

(defn- read-atom-string [a]
  (-> a
      validate-string-atom
      (string/replace  #"^\"" "")
      (string/replace #"\"$" "")
      (string/replace "\\\"" "\"")
      (string/replace "\\n" "\n")
      (string/replace "\\\\" "\\")))

(defn read-atom* [token]
  (cond
    (re-matches #"-?\d+" token) (Long/parseLong token)
    (re-matches #"-?\d+(\.\d+)?" token) (Double/parseDouble token)
    (#{"true" "false"} token) (Boolean/parseBoolean token)
    (= "nil" token) nil
    (re-find #"^\"" token) (read-atom-string token)
    (re-matches #"[^\s\[\]{}('\"`,;)]*" token) (symbol token) ; TODO remove regex duplication
    :else (throw (ex-info "Reader can't read token. Unknown format."
                          {:error ::token-unreadable
                           :token token}))))

(defn read-atom [[head & remaining]]
  {:result (read-atom* head)
   :tokens remaining})

(defn- validate-matching-paren [tokens]
  (when (nil? tokens)
    (throw (ex-info "EOF Exception. Unmatched parenthesis." {}))))

(defn read-collection [separator tokens]
  (loop [tokens (rest tokens) ; pop off open paren
         result []]
    (validate-matching-paren tokens)
    (let [[head & remaining] tokens]
      (cond
        (not= separator head) (let [form (read-form* tokens)]
                                (recur (:tokens form)
                                       (conj result (:result form))))
        (= separator head) {:result result
                            :tokens remaining}))))

(defn read-list
  "Explicitly coerce the collection into list.
  This is to carry the type information in the data structure itself.
  This might be too brittle, since manipulating seq will destroy this information."
  [tokens]
  (-> (read-collection ")" tokens)
      (update :result #(apply list %))))

(defn read-vector
  [tokens]
  (-> (read-collection "]" tokens)
      (update :result vec)))

(defn validate-even-number [coll]
  (if (even? (count coll))
    coll
    (throw (ex-info "The number of elements in the map is not even"
                    {:error ::map-odd-elements
                     :elements coll}))))

(defn read-map
  [tokens]
  (-> (read-collection "}" tokens)
      (validate-even-number)
      (update :result #(apply hash-map %))))

(def ^:private reader-macro->symbol
  {"'" 'quote
   "`" 'quasiquote
   "~" 'unquote
   "~@" 'splice-unquote
   "@" 'deref
   "^" 'with-meta})

(def ^:private reader-macro? (set (keys reader-macro->symbol)))

(defn- combine-read [state1 state2]
  {:result (conj (:result state1) (:result state2))
   :tokens (:tokens state2)})

(defn- read-forms [n tokens]
  (loop [i n
         state {:tokens tokens :result []}]
    (if (pos? i)
      (recur (dec i) (combine-read state (read-form* (:tokens state))))
      state)))

(defn- read-macro-with-meta [sym tokens]
  (let [{:keys [result tokens]} (read-forms 2 tokens)]
    {:result (apply list sym (reverse result))
     :tokens tokens}))

(defn- read-macro-generic [sym tokens]
  (let [{:keys [result tokens]} (read-form* tokens)]
    {:result (list sym result)
     :tokens tokens}))

(defn- read-reader-macro [tokens]
  (let [sym (reader-macro->symbol (first tokens))]
    (if (= sym 'with-meta)
      (read-macro-with-meta sym (rest tokens))
      (read-macro-generic sym (rest tokens)))))

(defn read-form* [tokens]
  (let [token (first tokens)]
    (cond
      (#{")" "]" "}"} token) (throw (ex-info "Unbalanced parenthesis"
                                             {:error ::unbalanced-parenthesis
                                              :remaining-tokens tokens}))
      (= "(" token) (read-list tokens)
      (= "[" token) (read-vector tokens)
      (= "{" token) (read-map tokens)
      (reader-macro? token) (read-reader-macro tokens)
      ; simplification: in case of loose tokens to evaluate, read the first token only, ignore the rest
      :else (read-atom tokens))))

(defn read-form [tokens]
  (:result (read-form* tokens)))

(defn read-str-fn
  "Choose which read-form implementation to use"
  [read-form-fn]
  (fn [s]
    (read-form-fn (tokenize s))))

(defn read-str [s]
  ((read-str-fn read-form) s))

(comment (read-form ["'" "(" "1" "2" ")"]))
(comment (read-form ["-2.1"]))
(comment (read-form ["(" ")"]))
(comment (read-form ["(" "1" "2" ")"]))
(comment (read-form ["(" "0" "(" "1" "4" ")" "2" "3" ")"]))

(comment (read-str "(0 (1 2) 3 4 (5) 6)"))

(comment (tokenize " ( 123 \"abc\" ) ")
         (tokenize "123"))
