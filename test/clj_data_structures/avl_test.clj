(ns clj-data-structures.avl-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clj-data-structures.avl :as avl]
            [clj-data-structures.bst :as bst]))

(defn- build-tree [& values]
  (reduce avl/insert (avl/make) values))

(deftest make-test
  (testing "empty tree has nil root"
    (is (nil? (:root (avl/make)))))

  (testing "default tree uses < and number?"
    (let [t (avl/make)]
      (is (= < (:to-left? t)))
      (is (= number? (:pred t)))))

  (testing "from a collection inserts all values"
    (let [t (avl/make [5 3 7])]
      (are [v] (bst/member? t v)
        5 3 7)))

  (testing "from a collection produces a balanced tree"
    (is (avl/balanced? (avl/make [5 3 7 1 4]))))

  (testing "from an empty collection produces an empty tree"
    (is (nil? (:root (avl/make [])))))

  (testing "throws when vals is not a collection"
    (is (thrown? Exception (avl/make 5)))))

(deftest insert-test
  (testing "tree is balanced after every insert"
    (is (avl/balanced? (build-tree 5 4 3 10 9 11))))

  (testing "left-heavy insertion triggers right rotation"
    (let [t (build-tree 5 3 1)]
      (is (avl/balanced? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "right-heavy insertion triggers left rotation"
    (let [t (build-tree 1 3 5)]
      (is (avl/balanced? t))
      (is (= 3 (get-in t [:root :value])))))

  (testing "left-right case triggers double rotation"
    (let [t (build-tree 5 3 4)]
      (is (avl/balanced? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "right-left case triggers double rotation"
    (let [t (build-tree 3 5 4)]
      (is (avl/balanced? t))
      (is (= 4 (get-in t [:root :value])))))

  (testing "all inserted values are members"
    (let [values [5 4 3 10 9 11 1 2]
          t      (apply build-tree values)]
      (are [v] (bst/member? t v)
        5 4 3 10 9 11 1 2)))

  (testing "sorted order is preserved after balancing"
    (is (= [1 3 4 5 9 10 11] (bst/tree->list (build-tree 5 4 3 10 9 11 1))))))

(deftest height-test
  (testing "empty tree has height 0"
    (is (= 0 (avl/height (avl/make)))))

  (testing "single node has height 1"
    (is (= 1 (avl/height (build-tree 5)))))

  (testing "balanced tree height is logarithmic"
    (let [t (avl/make [1 2 3 4 5 6 7])]
      (is (= 3 (avl/height t)))))

  (testing "avl height is lower than equivalent bst"
    (let [values  [1 2 3 4 5 6 7]
          avl-t   (avl/make values)
          bst-t   (bst/make values)]
      (is (< (avl/height avl-t) (avl/height bst-t))))))

(deftest balanced?-test
  (testing "empty tree is balanced"
    (is (avl/balanced? (avl/make))))

  (testing "single node is balanced"
    (is (avl/balanced? (build-tree 5))))

  (testing "avl tree is always balanced after inserts"
    (is (avl/balanced? (build-tree 5 4 3 10 9 11 1 2))))

  (testing "unbalanced bst is detected as unbalanced"
    (let [t (bst/make [1 2 3 4 5])]
      (is (not (avl/balanced? t))))))

(deftest balance-test
  (testing "balancing an already balanced tree returns equivalent tree"
    (let [t (build-tree 5 3 7)]
      (is (= (bst/tree->list t)
             (bst/tree->list (avl/balance t))))))

  (testing "balancing a skewed bst produces a balanced tree"
    (let [t (bst/make [1 2 3 4 5])]
      (is (not (avl/balanced? t)))
      (is (avl/balanced? (avl/balance t))))))
