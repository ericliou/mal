(ns step5-tco-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [step5-tco :as sut]
            [env]
            [clojure.test :refer :all]))

(deftest rep-test
  (testing "simple sexps"
    (are [s evaluation] (= evaluation
                           (sut/eval* (env/init-root-env!)
                                      (sut/read* s)))
      "(+ 1 2)" 3
      "{1 (+ 1 2)}" {1 3}
      "(do (def! x 2) (+ 2 x))" 4
      "[1 2 (+ 1 2)]" [1 2 3]
      "(if true 1 2)" 1
      "(if false 1 2)" 2
      "(let* [a (+ 2 2)] (+ 3 a))" 7
      "((fn* (a) (+ a 1)) 2)" 3))

  (testing "lambda fn* doing def! inside body"
    (let [root-env (env/init-root-env!)]
      (is (= 1
             (do (sut/eval* root-env
                  (sut/read* "((fn* (a) (def! x a)) 1)"))
                 (env/get-sym root-env 'x)))))

    (testing "lambda fn* doesn't mess with defs defined beforehand"
      (let [root-env (env/init-root-env!)]
        (is (= 1
              (do (sut/eval* root-env
                   (sut/read* "((fn* (a) (def! x a)) (def! y 1))"))
                  (env/get-sym root-env 'y)))))))

  (testing "let* doesn't leave dirty bindings outside of scope"
      (let [s "(let* [a 1] a)"
            root-env (env/init-root-env!)
            _ (sut/eval* root-env
                         (sut/read* s))]
        (is (thrown? Exception (env/get-sym (env/new-env nil) 'a)))))

  (testing "equality"
    (is (= false
           (sut/eval*
            (env/init-root-env!)
            (sut/read* "(= (list) nil)")))))

  (testing "recursive fn"
    (is (= nil
           (sut/eval*
            (env/init-root-env!)
            (sut/read* "(let* (cst (fn* (n)
                                   (if (= n 0) nil (cst (- n 1)))))
                              (cst 1))")))))
  (testing "variadic function"
    (is (= 3
           (sut/eval*
            (env/init-root-env!)
            (sut/read* "((fn* (& more) (count more)) 1 2 3)"))))))
