(ns bridle.constraint-test
  (:use [bridle.constraint])
  (:use [clojure.test]))

(deftest constrain-map-test
  (testing "optional map keys"
    (let [p (constrain-pred-fn (constrain-map :optional [:a :b]))]
      (is (fn? p))
      (is (p {:a 1}) "succeed for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")))
  (testing "optional limited map keys"
    (let [p (constrain-pred-fn
             (constrain-map :optional [:a :b] :allow-other-keys false))]
      (is (fn? p))
      (is (p {:a 1}) "succeed for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (not (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "mandatory keys"
    (let [p (constrain-pred-fn
             (constrain-map :mandatory [:a :b]))]
      (is (fn? p))
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2}) "fail for partial keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")))
  (testing "madatoy limited map keys"
    (let [p (constrain-pred-fn
             (constrain-map
              :mandatory [:a :b] :allow-other-keys false))]
      (is (fn? p))
      (is (not (p {:a 1})) "fail for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (not (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "nested keys"
    (let [p1 (constrain-pred-fn
              (constrain-map :mandatory [:a :b] :allow-other-keys false))
          p2 (constrain-pred-fn
              (constrain-map :optional [:c :d] :types {:c p1}))]
      (is (fn? p1))
      (is (fn? p2))
      (is (p2 {}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2}}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2} :d 1}) "succeed for all keys")
      (is (not (p2 {:c {:a 1 :b 2 :c 3} :d 1})) "fail for extra nested keys"))))
