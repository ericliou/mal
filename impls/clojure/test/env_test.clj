(ns env-test
  #_{:clj-kondo/ignore [:refer-all]}
  (:require [env :as sut]
            [clojure.test :refer :all]))

(deftest env-test
  (testing "simple put and get"
    (let [result (-> (sut/new-env nil)
                     (sut/put-sym! 'a 1)
                     (sut/get-sym 'a))]
      (is (= 1 result))))

  (testing "get symbol in parent"
    (let [result (-> (sut/new-env nil)
                     (sut/put-sym! 'a 1)
                     (sut/new-env)
                     (sut/get-sym 'a))]
      (is (= 1 result))))

  (testing "override symbol in new env"
    (let [result (-> (sut/new-env nil)
                     (sut/put-sym! 'a 1)
                     (sut/new-env)
                     (sut/put-sym! 'a 2)
                     (sut/get-sym 'a))]
      (is (= 2 result))))

  (testing "put-root-sym!"
    (let [child-env (-> (sut/init-root-env!)
                        (sut/put-sym! 'a 1)
                        (sut/new-env))
          _ (sut/put-root-sym! child-env 'b 2)
          result-a (sut/get-sym child-env 'a)
          result-b (sut/get-sym child-env 'b)]
      (is (= 1 result-a))
      (is (= 2 result-b))))

  (testing "symbol not found"
    (is (thrown? Exception
                 (-> (sut/new-env nil)
                     (sut/get-sym 'b))))))
