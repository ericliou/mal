(ns printer-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [printer]
            [clojure.test :refer :all]))

(deftest abs->str-test
  (testing "printing correctly"
    (are [abs result] (= result (printer/abs->str abs))
      'abc "abc"
      '+ "+"
      123 "123"
      [1] "(1)"
      [1 2 3] "(1 2 3)"
      ['+ ['- 1 2] 1 2 3] "(+ (- 1 2) 1 2 3)")))

