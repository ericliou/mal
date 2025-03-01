(ns reader-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [reader]
            [clojure.test :refer :all]))

(deftest read-str-test
  (testing "parsing of string"
    (are [s result] (= result (reader/read-str s))
      "\"I'm a string\"" "I'm a string"
      "\"\\n\"" "\n" ; new line
      "\" \\\" \"" " \" " ; double-quote
      "\"\\\\\"" "\\" ; escape backslash
      )

    (testing "EOF"
      (are [s] (thrown? Exception (reader/read-str s))
        "\""
        "\"\\\""
        "\"\\\"")))

  (testing "parsing of numbers"
    (are [s result] (= result (reader/read-str s))
      "0" 0
      "123" 123
      "-1.3" -1.3
      " -1.3 " -1.3))
  (testing "parsing of lists"
    (is (list? (reader/read-str "(1 2 3)")))
    (are [s result] (= result (reader/read-str s))
      "()" []
      " ( 1 ) " [1]
      "((2) (3 4) 5 (6))" [[2] [3 4] 5 [6]]))
  (testing "parsing of vectors"
    (is (vector? (reader/read-str "[1 2 3]")))
    (are [s result] (= result (reader/read-str s))
      "[]" []
      " [ 1 ] " [1]
      "[[2] (3 4) 5 [6]]" [[2] '(3 4) 5 [6]]))
  (testing "parsing of maps"
    (is (map? (reader/read-str " {1 2 3 4} ")))
    (are [s result] (= result (reader/read-str s))
      " {} " {}
      " {\"1\" 2 3 4}" {"1" 2 3 4}
      "{1 a 2 [3 4] 0 {5 6}}" {1 'a 2 [3 4] 0 {5 6}}))
  (testing "parsing of macros"
    (are [s result] (= result (reader/read-str s))
      "'(1 2)" ['quote [1 2]]
      "~@(1 2)" ['splice-unquote [1 2]]
      "`(1 ~a 3)" ['quasiquote [1 ['unquote 'a] 3]]
       "^{\"a\" 1} [1 2 3]" '(with-meta [1 2 3] {"a" 1})))
  (testing "other parsing"
    (are [s result] (= result (reader/read-str s))
      "true" true
      "false" false
      "nil" nil
      "abc" 'abc
      "a-b-c" 'a-b-c)))
