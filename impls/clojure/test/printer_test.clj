(ns printer-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [printer]
            [clojure.test :refer :all]))

(deftest abs->str-test
  (testing "printing correctly"
    (are [abs result] (= result (printer/abs->string abs))
      'abc "abc"
      '+ "+"
      123 "123"
      nil "nil"
      "hi\nhi\"ho" "\"hi\\nhi\\\"ho\""
      [1] "(1)"
      [1 2 nil] "(1 2 nil)"
      ['+ ['- 1 2] 1 2 3] "(+ (- 1 2) 1 2 3)")))

