(ns reader-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [reader]
            [clojure.test :refer :all]))

(deftest read-str-test
  (testing "parsing of string"
    (are [s result] (= result (reader/read-str s))
      "\"I'm a string\"" "I'm a string"
      "\"\\n\"" "\n"
      "\" \\\" \"" " \" "))
  (testing "parsing of numbers"
    (are [s result] (= result (reader/read-str s))
      "0" 0
      "123" 123
      "-1.3" -1.3
      " -1.3 " -1.3))
  (testing "parsing of lists"
    (are [s result] (= result (reader/read-str s))
      "()" []
      " ( 1 ) " [1]
      "((2) (3 4) 5 (6))" [[2] [3 4] 5 [6]]))
  (testing "parsing of macros"
    (are [s result] (= result (reader/read-str s))
      "'(1 2)" ['quote [1 2]]
      "~@(1 2)" ['splice-unquote [1 2]]
      "`(1 ~a 3)" ['quasiquote [1 ['unquote 'a] 3]]))
  (testing "other parsing"
    (are [s result] (= result (reader/read-str s))
      "true" true
      "false" false
      "nil" nil
      "abc" 'abc
      "a-b-c" 'a-b-c)))

