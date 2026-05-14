(ns clj-data-structures.brt-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clj-data-structures.brt :as brt]
            [clj-data-structures.test-aux :as aux]))

(deftest make-test
  (testing "empty tree has nil root"
    (is (nil? (:root (brt/make!)))))

  (testing "default tree uses < and number?"
    (let [t (brt/make!)]
      (is (= < (:to-left? t)))
      (is (= number? (:pred t)))))

  (testing "from a collection inserts all values"
    (let [t (brt/make! [5 3 7])]
      (are [v] (brt/member? t v)
        5 3 7)))

  (testing "from a collection produces a valid rbt"
    (is (aux/brt-valid? (brt/make! [5 3 7 1 4]))))

  (testing "from an empty collection produces an empty tree"
    (is (nil? (:root (brt/make! [])))))

  (testing "root is always black"
    (is (= :black (get-in (brt/make! [5 3 7]) [:root :colour]))))

  (testing "throws when vals is not a collection"
    (is (thrown? Exception (brt/make! 5)))))

(deftest insert-test
  (testing "tree is valid after every insert"
    (is (aux/brt-valid? (brt/make! [5 4 3 10 9 11]))))

  (testing "root is always black after insert"
    (let [t (-> (brt/make!) (brt/insert! 5) (brt/insert! 3))]
      (is (= :black (get-in t [:root :colour])))))

  (testing "ll case triggers right rotation"
    (let [t (brt/make! [5 3 1])]
      (is (aux/brt-valid? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "lr case triggers double rotation"
    (let [t (brt/make! [5 3 4])]
      (is (aux/brt-valid? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "rr case triggers left rotation"
    (let [t (brt/make! [1 3 5])]
      (is (aux/brt-valid? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "rl case triggers double rotation"
    (let [t (brt/make! [3 5 4])]
      (is (aux/brt-valid? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "all inserted values are members"
    (let [t (brt/make! [5 4 3 10 9 11 1 2])]
      (are [v] (brt/member? t v)
        5 4 3 10 9 11 1 2)))

  (testing "sorted order is preserved after rebalancing"
    (is (= [1 3 4 5 9 10 11] (brt/tree->list (brt/make! [5 4 3 10 9 11 1])))))

  (testing "insert does not mutate — returns a new tree"
    (let [t  (brt/make!)
          t1 (brt/insert! t 5)
          t2 (brt/insert! t1 3)]
      (is (nil? (:root t)))
      (is (nil? (get-in t1 [:root :left])))))

  (testing "large insertion sequence produces valid rbt"
    (is (aux/brt-valid? (brt/make! (range 1 20)))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (brt/insert! (brt/make!) "a")))))

(deftest member?-test
  (testing "present values are found"
    (let [t (brt/make! [5 3 7 1 4])]
      (are [v] (brt/member? t v)
        5 3 7 1 4)))

  (testing "absent values are not found"
    (let [t (brt/make! [5 3 7])]
      (are [v] (not (brt/member? t v))
        0 4 6 9)))

  (testing "always false on empty tree"
    (is (false? (brt/member? (brt/make!) 1))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (brt/member? (brt/make! [5]) "x")))))

(deftest tree->list-test
  (testing "empty tree returns nil"
    (is (nil? (brt/tree->list (brt/make!)))))

  (testing "single element"
    (is (= [5] (brt/tree->list (brt/make! [5])))))

  (testing "returns values in ascending order"
    (is (= [1 3 4 5 7] (brt/tree->list (brt/make! [5 3 7 1 4])))))

  (testing "order is independent of insertion order"
    (is (= (brt/tree->list (brt/make! [5 3 7 1 4]))
           (brt/tree->list (brt/make! [1 7 3 4 5]))))))

(deftest size-test
  (testing "empty tree has size 0"
    (is (= 0 (brt/size (brt/make!)))))

  (testing "single node has size 1"
    (is (= 1 (brt/size (brt/make! [5])))))

  (testing "size equals number of inserted elements"
    (is (= 5 (brt/size (brt/make! [5 3 7 1 4]))))))

(deftest min-val-test
  (testing "nil on empty tree"
    (is (nil? (brt/min-val (brt/make!)))))

  (testing "min is correct after rebalancing"
    (is (= 1 (brt/min-val (brt/make! [5 3 7 1 4]))))))

(deftest max-val-test
  (testing "nil on empty tree"
    (is (nil? (brt/max-val (brt/make!)))))

  (testing "max is correct after rebalancing"
    (is (= 7 (brt/max-val (brt/make! [5 3 7 1 4]))))))
