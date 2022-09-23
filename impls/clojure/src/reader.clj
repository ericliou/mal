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

(defn read-token [token]
  (cond
    (re-matches #"-?\d+" token) (Long/parseLong token)
    (re-matches #"-?\d+(\.\d+)?" token) (Double/parseDouble token)
    :else (throw (ex-info "Reader can't read token. Unknown format." {:token token}))))

(comment
      (re-matches #"-?\d+" "+11")
      (clojure.edn/read-string "+11")
      (Float/parseFloat "-11")
)

(comment
  "(+ 1 (- 3 2 ) )"
  ;
  
  (rest [1 2 3])
  )

(defn tap [x] (prn x) x)

(defn read-list [tokens]
  (loop [tokens (rest tokens) ; pop off open paren
         result []]
    (prn "loop tokens" tokens)
    (prn "loop result" result)
    (let [token (first tokens)]
      (cond
        (not= ")" token) (let [read-result (read-form tokens)]
                           (prn "read-result" read-result)
                           (recur (:tokens read-result)
                                  (conj result (:result read-result))))
        (= ")" token) {:result result
                       :tokens (rest tokens)}

        ; else unbalanced if close-paren not found

        )))

  )

(defn read-form [tokens]
  (prn "read-form" tokens)
  (let [token (first tokens)]
    (cond
      (= "(" token) (read-list tokens)
      :else (tap {:result (read-token token)
                  :tokens (rest tokens)})
      )
    ))

(comment (read-form ["-2.1"])
         ; => -2.1
         )
(comment (read-form ["(" ")"])
         ; => ( 1 2 )
         )
(comment (read-form ["(" "1" "2" ")"])
         ; => ( 1 2 )
         )
(comment (read-form ["(" "0" "(" "1" "4" ")" "2" "3" ")"])
         ; => ( 1 2 )
         )
(comment *e)

(defn read-str [s]
  (let [tokens (tokenize s)]
    (read-form tokens)))

(comment (tokenize " ( 123 \"abc\" ) ")
         (tokenize "123"))

(comment (type 2.2))

