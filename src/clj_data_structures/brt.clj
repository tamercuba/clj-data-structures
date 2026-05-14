(ns clj-data-structures.brt
  (:require
   [clj-data-structures.bst :as bst]))

(defn- red-node [value]
  {:value value :left nil :right nil :colour :red})

(defn- double-blacken [node]
  (assoc node :colour :double-black))

(defn- blacken [node]
  (assoc node :colour :black))

(defn- reden [node]
  (assoc node :colour :red))

(defn- double-black? [{:keys [colour]}]
  (= colour :double-black))

(defn- black? [{:keys [colour]}]
  (= colour :black))

(defn- red? [{:keys [colour]}]
  (= colour :red))

(defn- rotate-left [{:keys [right] :as node}]
  (assoc right :left (assoc node :right (:left right))))

(defn- rotate-right [{:keys [left] :as node}]
  (assoc left :right (assoc node :left (:right left))))

(defn- ll-violation? [node]
  (and (black? node)
       (red?   (:left node))
       (red?   (-> node :left :left))))

(defn- lr-violation? [node]
  (and (black? node)
       (red?   (:left node))
       (red?   (-> node :left :right))))

(defn- rl-violation? [node]
  (and (black? node)
       (red?   (:right node))
       (red?   (-> node :right :left))))

(defn- rr-violation? [node]
  (and (black? node)
       (red?   (:right node))
       (red?   (-> node :right :right))))

(defn- balance [node]
  (cond
    (ll-violation? node) (-> node
                             rotate-right
                             (update :left blacken))
    (lr-violation? node) (-> node
                             (update :left rotate-left)
                             rotate-right
                             (update :left blacken))
    (rr-violation? node) (-> node
                             rotate-left
                             (update :right blacken))
    (rl-violation? node) (-> node
                             (update :right rotate-right)
                             rotate-left
                             (update :right blacken))
    :else                node))

(defn- insert-node [node value to-left? node-factory]
  (cond
    (nil? node)                          (node-factory value)
    (to-left?      value (:value node))  (balance (update node :left  insert-node value to-left? node-factory))
    (not (to-left? value (:value node))) (balance (update node :right insert-node value to-left? node-factory))
    :else                                node))

(defn insert!
  "Inserts value into tree and rebalances to preserve red-black invariants.
  The root is always black after insertion. Throws ex-info if value does not
  satisfy the tree's pred."
  [{:keys [root pred to-left? node-factory] :as tree} value]
  (if (pred value)
    (assoc tree :root (blacken (insert-node root value to-left? node-factory)))
    (throw (ex-info (str value " doesnt satisfy the pred" pred) {:key value}))))

(def member?
  "Returns true if search-value is present in tree, false otherwise.
  Throws ex-info if search-value does not satisfy the tree's pred."
  bst/member?)

(def min-val
  "Returns the minimum value in tree, or nil if the tree is empty."
  bst/min-val)

(def max-val
  "Returns the maximum value in tree, or nil if the tree is empty."
  bst/max-val)

(def tree->list
  "Returns a lazy sequence of the tree's values in ascending order (in-order
  traversal). Returns nil if the tree is empty."
  bst/tree->list)

(def size
  "Returns the number of nodes in tree."
  bst/size)

(defn make!
  "Creates a red-black tree. With no arguments, defaults to < as the ordering
  function and number? as the value predicate. Accepts an optional collection
  of initial values which are inserted and rebalanced in sequence."
  ([]                   (make! < number? [] red-node))
  ([vals]               (make! < number? vals))
  ([to-left? pred]      (make! to-left? pred [] red-node))
  ([to-left? pred vals] (make! to-left? pred vals red-node))
  ([to-left? pred
    vals node-factory]  (reduce insert!
                                {:to-left? to-left? :pred pred :node-factory node-factory :root nil}
                                (seq vals))))
