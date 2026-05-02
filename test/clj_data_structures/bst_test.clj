(ns clj-data-structures.bst-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clj-data-structures.bst :as bst]))

(defn- build-tree [& values]
  (reduce bst/insert (bst/make-tree) values))

(deftest make-tree-test
  (testing "empty tree has nil root"
    (is (nil? (:root (bst/make-tree)))))

  (testing "default tree uses < and number?"
    (let [t (bst/make-tree)]
      (is (= < (:to-left? t)))
      (is (= number? (:pred t)))))

  (testing "from a collection inserts all values"
    (let [t (bst/make-tree [5 3 7])]
      (are [v] (bst/member? t v)
        5 3 7)))

  (testing "from a collection produces a sorted tree"
    (is (= [1 3 5 7] (bst/tree->list (bst/make-tree [5 3 7 1])))))

  (testing "from an empty collection produces an empty tree"
    (is (nil? (:root (bst/make-tree [])))))

  (testing "from a collection with custom comparator and pred"
    (let [t (bst/make-tree #(< (count %1) (count %2)) string? ["hi" "hello" "x"])]
      (are [v] (bst/member? t v)
        "hi" "hello" "x")))

  (testing "throws when collection contains invalid values"
    (is (thrown? clojure.lang.ExceptionInfo (bst/make-tree [1 2 "a"]))))

  (testing "throws when vals is not a collection"
    (is (thrown? Exception (bst/make-tree 5)))))

(deftest insert-test
  (testing "first insert becomes the root"
    (let [t (bst/insert (bst/make-tree) 5)]
      (is (= 5 (get-in t [:root :value])))))

  (testing "lesser value goes left"
    (let [t (build-tree 5 3)]
      (is (= 3 (get-in t [:root :left :value])))))

  (testing "greater value goes right"
    (let [t (build-tree 5 7)]
      (is (= 7 (get-in t [:root :right :value])))))

  (testing "insert does not mutate — returns a new tree"
    (let [t  (bst/make-tree)
          t1 (bst/insert t 5)
          t2 (bst/insert t1 3)]
      (is (nil? (:root t)))
      (is (nil? (get-in t1 [:root :left])))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo (bst/insert (bst/make-tree) "a"))))

  (testing "exception carries the offending value"
    (try
      (bst/insert (bst/make-tree) :bad)
      (catch clojure.lang.ExceptionInfo e
        (is (= :bad (:key (ex-data e))))))))

(deftest member?-test
  (testing "present values are found"
    (let [t (build-tree 5 3 7 1 4)]
      (are [v] (bst/member? t v)
        5 3 7 1 4)))

  (testing "absent values are not found"
    (let [t (build-tree 5 3 7)]
      (are [v] (not (bst/member? t v))
        0 4 6 9)))

  (testing "always false on empty tree"
    (is (false? (bst/member? (bst/make-tree) 1))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo
                 (bst/member? (build-tree 5) "x")))))

(deftest remove-test
  (testing "removed value is no longer a member"
    (let [t (-> (build-tree 5 3 7) (bst/remove 3))]
      (is (false? (bst/member? t 3)))))

  (testing "other values survive removal"
    (let [t (-> (build-tree 5 3 7 1 4) (bst/remove 3))]
      (are [v] (bst/member? t v)
        5 7 1 4)))

  (testing "remove leaf"
    (let [t (-> (build-tree 5 3) (bst/remove 3))]
      (is (nil? (get-in t [:root :left])))))

  (testing "remove node with one child"
    (let [t (-> (build-tree 5 3 1) (bst/remove 3))]
      (is (bst/member? t 1))
      (is (false? (bst/member? t 3)))))

  (testing "remove node with two children"
    (let [t (-> (build-tree 5 3 7 1 4) (bst/remove 3))]
      (is (bst/member? t 4))
      (is (false? (bst/member? t 3)))))

  (testing "remove root"
    (let [t (-> (build-tree 5 3 7) (bst/remove 5))]
      (is (false? (bst/member? t 5)))
      (is (bst/member? t 3))
      (is (bst/member? t 7))))

  (testing "returns a tree map, not a bare node"
    (let [t (-> (build-tree 5 3) (bst/remove 3))]
      (is (contains? t :root))
      (is (contains? t :pred))
      (is (contains? t :to-left?))))

  (testing "throws when value does not satisfy pred"
    (is (thrown? clojure.lang.ExceptionInfo
                 (bst/remove (build-tree 5) "x")))))

(deftest tree->list-test
  (testing "empty tree returns nil"
    (is (nil? (bst/tree->list (bst/make-tree)))))

  (testing "single element"
    (is (= [5] (bst/tree->list (build-tree 5)))))

  (testing "returns values in ascending order"
    (is (= [1 3 4 5 7] (bst/tree->list (build-tree 5 3 7 1 4)))))

  (testing "order is independent of insertion order"
    (is (= (bst/tree->list (build-tree 5 3 7 1 4))
           (bst/tree->list (build-tree 1 7 3 4 5)))))

  (testing "in-order after remove is still sorted"
    (let [t (-> (build-tree 5 3 7 1 4) (bst/remove 3))]
      (is (= [1 4 5 7] (bst/tree->list t))))))

(deftest min-val-test
  (testing "min of single node is itself"
    (is (= 5 (bst/min-val (build-tree 5)))))

  (testing "min is the leftmost value"
    (is (= 1 (bst/min-val (build-tree 5 3 7 1 4)))))

  (testing "nil on empty tree"
    (is (nil? (bst/min-val (bst/make-tree)))))

  (testing "min after removing the minimum"
    (is (= 3 (bst/min-val (-> (build-tree 5 3 7 1) (bst/remove 1)))))))

(deftest max-val-test
  (testing "max of single node is itself"
    (is (= 5 (bst/max-val (build-tree 5)))))

  (testing "max is the rightmost value"
    (is (= 7 (bst/max-val (build-tree 5 3 7 1 4)))))

  (testing "nil on empty tree"
    (is (nil? (bst/max-val (bst/make-tree)))))

  (testing "max after removing the maximum"
    (is (= 5 (bst/max-val (-> (build-tree 5 3 7) (bst/remove 7)))))))
