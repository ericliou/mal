(ns step4-if-fn-do-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [step4-if-fn-do :as sut]
            [clojure.test :refer :all]))

(deftest rep-test
  (testing "simple sexps"
    (are [s evaluation] (= evaluation
                           (-> {:env sut/repl-env :ast (sut/read* s)}
                               sut/eval*
                               :evaluation))
      "(+ 1 2)" 3
      "{1 (+ 1 2)}" {1 3}
      "(do (def! x 2) (+ 2 x))" 4
      "[1 2 (+ 1 2)]" [1 2 3]
      "(if true 1 2)" 1
      "(if false 1 2)" 2
      "(let* [a (+ 2 2)] (+ 3 a))" 7
      "((fn* (a) (+ a 1)) 2)" 3))

  #_(testing "let* removes binding after scope"
    (let [s "(let* [a 1] a)"
          {:keys [env]} (sut/eval* {:env sut/repl-env
                             :ast (sut/read* s)})]
    (is (nil? (get env 'a))))))
