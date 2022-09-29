(ns reader-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [reader]
            [clojure.test :refer :all]))

(deftest read-str-test
  (testing "correctly parsing"
    (are [s result] (= result (reader/read-str s))
         "0" 0
         "true" true
         "false" false
         "-1.3" -1.3
         " -1.3 " -1.3
         "abc" 'abc
         "\"I'm a string\"" "I'm a string"
         "\"\\n\"" "\n"
         "\" \\\" \"" " \" "
         "()" []
         " ( 1 ) " [1]
         "((2) (3 4) 5 (6))" [[2] [3 4] 5 [6]]
         ))
  
  (testing "parsing with different impl: read-form2"
    (are [s result] (= result ((reader/read-str-fn reader/read-form2) s))
         "0" 0
         "-1.3" -1.3
         "()" []
         "(1)" [1]
         "((2) (3 4) 5 (6))" [[2] [3 4] 5 [6]]
         )))

