(ns step4-if-fn-do-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [step4-if-fn-do :as sut]
            [clojure.test :refer :all]))

(deftest rep-test
  (testing "simple sexps"
    (are [s evaluation] (= evaluation (sut/eval* {} (sut/read* s)))
      "(+ 1 2)" 3
      "{1 (+ 1 2)}" {1 3}
      "(do (def! x 2) (+ 2 x))" 4
      "[1 2 (+ 1 2)]" [1 2 3]
      "(if true 1 2)" 1
      "(if false 1 2)" 2
      "(let* [a (+ 2 2)] (+ 3 a))" 7
      "((fn* (a) (+ a 1)) 2)" 3))

  (testing "lambda fn* doing def! inside body"
    (is (= 1
           (do (sut/eval* {} (sut/read* "((fn* (a) (def! x a)) 1)"))
               (sut/get-sym {} 'x))))

    (testing "lambda fn* doesn't touch defs done before it"
      (is (= 1
             (do (sut/eval* {} (sut/read* "((fn* (a) (def! x a)) (def! y 1))"))
                 (sut/get-sym {} 'y))))))

  (testing "let* doesn't leave dirty bindings outside of scope"
      (let [s "(let* [a 1] a)"
            _ (sut/eval* {} (sut/read* s))]
        (is (thrown? Exception (sut/get-sym {} 'a)))))

  (testing "equality"
    (is (= false
           (sut/eval*
            {}
            (sut/read* "(= (list) nil)"))))))

