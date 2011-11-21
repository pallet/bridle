(ns bridle.constraint-test
  (:use [bridle.constraint])
  (:use [clojure.test]))

(deftest constrain-map-test
  (testing "optional map keys"
    (let [p (constrain-pred-fn (constrain-map :optional [:a :b]))]
      (is (fn? p))
      (is (p {:a 1}) "succeed for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")
      (is (not (p [:a 1 :b 2])) "fail on arbitrary data")
      (is (not (p 1)) "fail on arbitrary data")))
  (testing "optional limited map keys"
    (let [p (constrain-pred-fn
             (constrain-map :optional [:a :b] :allow-other-keys false))]
      (is (fn? p))
      ;; (is (p {:a 1}) "succeed for partial keys") ;; fails pending MATCH-39
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (not (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "has keys"
    (let [p (constrain-pred-fn
             (constrain-map :has [:a :b]))]
      (is (fn? p))
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (p {:a 1 :b 2}) "fail for partial keys")
      (is (p {:a 1 :b 2 :c 3}) "succeed for extra keys")
      (is (not (p [:a 1 :b 2])) "fail on arbitrary data")))
  (testing "madatoy limited map keys"
    (let [p (constrain-pred-fn
             (constrain-map
              :has [:a :b] :allow-other-keys false))]
      (is (fn? p))
      (is (not (p {:a 1})) "fail for partial keys")
      (is (p {:a 1 :b 2}) "succeed for all keys")
      (is (not (p {:a 1 :b 2 :c 3})) "fail for extra keys")))
  (testing "nested keys"
    (let [p1 (constrain-pred-fn
              (constrain-map :has [:a :b] :allow-other-keys false))
          p2 (constrain-pred-fn
              (constrain-map :optional [:c :d] :types {:c p1}))]
      (is (fn? p1))
      (is (fn? p2))
      (is (p2 {}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2}}) "succeed for partial keys")
      (is (p2 {:c {:a 1 :b 2} :d 1}) "succeed for all keys")
      (is (not (p2 {:c {:a 1 :b 2 :c 3} :d 1})) "fail for extra nested keys"))))

(defconstraint p1 (constrain-map :optional [:a :b]))

(defmacro fails
  [expr msg]
  `(let [a# (atom nil)]
     (binding [constraint-failed-handler (fn [_# _#] (reset! a# true))]
       ~expr
       (is @a# ~msg))))

(deftest defconstraint-test
  (testing "optional map keys"
    (is (fn? p1))
    (is (p1 {:a 1}) "succeed for partial keys")
    (is (p1 {:a 1 :b 2}) "succeed for all keys")
    (is (p1 {:a 1 :b 2 :c 3}) "succeed for extra keys")
    (fails (p1 1) "fail for arbitrary data")))
