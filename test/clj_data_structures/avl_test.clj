(ns clj-data-structures.avl-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clj-data-structures.avl :as avl]
            [clj-data-structures.test-aux :as aux]))

(deftest make-test
  (testing "empty tree has nil root"
    (is (nil? (:root (avl/make!)))))

  (testing "default tree uses < and number?"
    (let [t (avl/make!)]
      (is (= < (:to-left? t)))
      (is (= number? (:pred t)))))

  (testing "from a collection inserts all values"
    (let [t (avl/make! [5 3 7])]
      (are [v] (avl/member? t v)
        5 3 7)))

  (testing "from a collection produces a balanced tree"
    (is (aux/avl-balanced? (avl/make! [5 3 7 1 4]))))

  (testing "from an empty collection produces an empty tree"
    (is (nil? (:root (avl/make! [])))))

  (testing "throws when vals is not a collection"
    (is (thrown? Exception (avl/make! 5)))))

(deftest insert-test
  (testing "tree is balanced after every insert"
    (is (aux/avl-balanced? (avl/make! [5 4 3 10 9 11]))))

  (testing "left-heavy insertion triggers right rotation"
    (let [t (avl/make! [5 3 1])]
      (is (aux/avl-balanced? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "right-heavy insertion triggers left rotation"
    (let [t (avl/make! [1 3 5])]
      (is (aux/avl-balanced? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "left-right case triggers double rotation"
    (let [t (avl/make! [5 3 4])]
      (is (aux/avl-balanced? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "right-left case triggers double rotation"
    (let [t (avl/make! [3 5 4])]
      (is (aux/avl-balanced? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "all inserted values are members"
    (let [t (avl/make! [5 4 3 10 9 11 1 2])]
      (are [v] (avl/member? t v)
        5 4 3 10 9 11 1 2)))

  (testing "sorted order is preserved after balancing"
    (is (= [1 3 4 5 9 10 11] (avl/tree->list (avl/make! [5 4 3 10 9 11 1])))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (avl/insert! (avl/make!) "a")))))

(deftest height-test
  (testing "empty tree has height 0"
    (is (= 0 (avl/height (avl/make!)))))

  (testing "single node has height 1"
    (is (= 1 (avl/height (avl/make! [5])))))

  (testing "balanced tree height is logarithmic"
    (is (= 3 (avl/height (avl/make! [1 2 3 4 5 6 7])))))

  (testing "avl height grows logarithmically, not linearly"
    (let [values [1 2 3 4 5 6 7]]
      (is (< (avl/height (avl/make! values))
             (count values))))))

(deftest balanced?-test
  (testing "empty tree is balanced"
    (is (aux/avl-balanced? (avl/make!))))

  (testing "single node is balanced"
    (is (aux/avl-balanced? (avl/make! [5]))))

  (testing "avl tree is always balanced after inserts"
    (is (aux/avl-balanced? (avl/make! [5 4 3 10 9 11 1 2]))))

  (testing "tree with out-of-range factor is detected as unbalanced"
    (let [leaf {:value 3 :left nil :right nil :height 1 :factor 0}
          root {:value 1 :left nil :right leaf :height 2 :factor -2}
          t    {:root root :pred number? :to-left? < :node-factory nil}]
      (is (not (aux/avl-balanced? t))))))

(deftest remove-test
  (testing "removed value is no longer a member"
    (let [t (-> (avl/make! [5 3 7]) (avl/remove! 3))]
      (is (false? (avl/member? t 3)))))

  (testing "tree is balanced after removing a leaf"
    (is (aux/avl-balanced? (-> (avl/make! [5 3 7 2 4]) (avl/remove! 2)))))

  (testing "tree is balanced after removing a node with one child"
    (let [t (-> (avl/make! [5 3 7 2]) (avl/remove! 3))]
      (is (aux/avl-balanced? t))
      (is (avl/member? t 2))))

  (testing "tree is balanced after removing a node with two children"
    (let [t (-> (avl/make! [5 3 7 2 4]) (avl/remove! 3))]
      (is (aux/avl-balanced? t))
      (is (avl/member? t 2))
      (is (avl/member? t 4))))

  (testing "tree is balanced after removing root"
    (let [t (-> (avl/make! [5 3 7]) (avl/remove! 5))]
      (is (aux/avl-balanced? t))
      (is (avl/member? t 3))
      (is (avl/member? t 7))))

  (testing "removal triggers rebalance"
    (is (aux/avl-balanced? (-> (avl/make! [5 3 7 2 4]) (avl/remove! 7)))))

  (testing "sorted order is preserved after removal"
    (is (= [1 4 5 7] (avl/tree->list (-> (avl/make! [5 3 7 1 4]) (avl/remove! 3))))))

  (testing "other values survive removal"
    (let [t (-> (avl/make! [5 4 3 10 9 11]) (avl/remove! 4))]
      (are [v] (avl/member? t v)
        5 3 10 9 11)))

  (testing "remove non-existent value returns unchanged tree"
    (let [t (avl/make! [5 3 7])]
      (is (= (avl/tree->list t)
             (avl/tree->list (avl/remove! t 99))))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (avl/remove! (avl/make! [5]) "a")))))

(deftest tree->list-test
  (testing "empty tree returns nil"
    (is (nil? (avl/tree->list (avl/make!)))))

  (testing "single element"
    (is (= [5] (avl/tree->list (avl/make! [5])))))

  (testing "returns values in ascending order"
    (is (= [1 3 4 5 7] (avl/tree->list (avl/make! [5 3 7 1 4])))))

  (testing "order is independent of insertion order"
    (is (= (avl/tree->list (avl/make! [5 3 7 1 4]))
           (avl/tree->list (avl/make! [1 7 3 4 5])))))

  (testing "in-order after remove is still sorted"
    (is (= [1 4 5 7] (avl/tree->list (-> (avl/make! [5 3 7 1 4]) (avl/remove! 3)))))))

(deftest size-test
  (testing "empty tree has size 0"
    (is (= 0 (avl/size (avl/make!)))))

  (testing "single node has size 1"
    (is (= 1 (avl/size (avl/make! [5])))))

  (testing "size equals number of inserted elements"
    (is (= 5 (avl/size (avl/make! [5 3 7 1 4])))))

  (testing "size decreases by 1 after remove"
    (let [t (avl/make! [5 3 7])]
      (is (= 2 (avl/size (avl/remove! t 3))))))

  (testing "size is correct after rebalancing"
    (is (= 6 (avl/size (avl/make! [1 2 3 4 5 6]))))))

(deftest balance-test
  (testing "balancing an already balanced tree returns equivalent tree"
    (let [t (avl/make! [5 3 7])]
      (is (= (avl/tree->list t)
             (avl/tree->list (avl/balance t))))))

  (testing "balancing a skewed tree produces a balanced tree"
    (let [n4   {:value 4 :left nil :right nil :height 1 :factor 0}
          n3   {:value 3 :left nil :right n4   :height 2 :factor -1}
          n2   {:value 2 :left nil :right n3   :height 3 :factor -2}
          root {:value 1 :left nil :right n2   :height 4 :factor -3}
          t    {:root root :pred number? :to-left? < :node-factory nil}]
      (is (not (aux/avl-balanced? t)))
      (is (aux/avl-balanced? (avl/balance t))))))

(deftest member?-test
  (testing "present values are found"
    (let [t (avl/make! [5 3 7 1 4])]
      (are [v] (avl/member? t v)
        5 3 7 1 4)))

  (testing "absent values are not found"
    (let [t (avl/make! [5 3 7])]
      (are [v] (not (avl/member? t v))
        0 4 6 9)))

  (testing "always false on empty tree"
    (is (false? (avl/member? (avl/make!) 1))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (avl/member? (avl/make! [5]) "x")))))

(deftest min-val-test
  (testing "nil on empty tree"
    (is (nil? (avl/min-val (avl/make!)))))

  (testing "min is the leftmost value after rebalancing"
    (is (= 1 (avl/min-val (avl/make! [5 3 7 1 4])))))

  (testing "min after removing the minimum"
    (is (= 3 (avl/min-val (-> (avl/make! [5 3 7 1]) (avl/remove! 1)))))))

(deftest max-val-test
  (testing "nil on empty tree"
    (is (nil? (avl/max-val (avl/make!)))))

  (testing "max is the rightmost value after rebalancing"
    (is (= 7 (avl/max-val (avl/make! [5 3 7 1 4])))))

  (testing "max after removing the maximum"
    (is (= 5 (avl/max-val (-> (avl/make! [5 3 7]) (avl/remove! 7)))))))
