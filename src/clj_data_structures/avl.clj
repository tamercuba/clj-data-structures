(ns clj-data-structures.avl
  (:refer-clojure :exclude [remove])
  (:require [clj-data-structures.bst :as bst]))

(defn- default-node-factory [value]
  {:value value :left nil :right nil :factor 0 :height 0})

(defn- node-height [node]
  (if (some? node) (:height node) 0))

(defn- update-node [node]
  (let [lh (node-height (:left node))
        rh (node-height (:right node))]
    (assoc node
           :height (+ 1 (max lh rh))
           :factor (- lh rh))))

(defn- balanced-node? [{:keys [left right factor] :as node}]
  (if (nil? node)
    true
    (and (< -2 factor 2)
         (balanced-node? left)
         (balanced-node? right))))

(defn- rotate-left [{:keys [right] :as node}]
  (let [new-left (update-node (assoc node :right (:left right)))]
    (update-node (assoc right :left new-left))))

(defn- rotate-right [{:keys [left] :as node}]
  (let [new-right (update-node (assoc node :left (:right left)))]
    (update-node (assoc left :right new-right))))

(defn- rotate-left-right [{:keys [left] :as node}]
  (-> node
      (assoc :left (rotate-left left))
      rotate-right))

(defn- rotate-right-left [{:keys [right] :as node}]
  (-> node
      (assoc :right (rotate-right right))
      rotate-left))

(defn- balance-nodes [{:keys [left right] :as node}]
  (let [node  (-> node
                  (assoc :left  (when left  (balance-nodes left)))
                  (assoc :right (when right (balance-nodes right)))
                  update-node)
        left  (:left node)
        right (:right node)
        nf    (:factor node)]
    (cond
      (and (> nf 1)  (>= (:factor left)  0)) (rotate-right node)
      (> nf 1)                                    (rotate-left-right node)
      (and (< nf -1) (<= (:factor right) 0)) (rotate-left node)
      (< nf -1)                                   (rotate-right-left node)
      :else                                        node)))

(defn height
  "Returns the height of the tree. Empty tree has height 0."
  [{:keys [root]}]
  (if (nil? root)
    0
    (node-height root)))

(defn balanced?
  "Returns true if every node in tree has a balance factor of at most 1."
  [{:keys [root]}]
  (if (nil? root)
    true
    (balanced-node? root)))

(defn balance
  "Rebalances tree bottom-up. Can be used to fix a BST that has become skewed."
  [{:keys [root] :as tree}]
  (if (nil? root) tree (assoc tree :root (balance-nodes root))))

(defn insert
  "Inserts value into tree and rebalances. Throws ex-info if value does not
  satisfy the tree's pred."
  [tree value]
  (-> tree
      (bst/insert value)
      balance))

(defn remove
  "Removes value from tree and rebalances. Throws ex-info if value does not
  satisfy the tree's pred."
  [tree value]
  (-> tree
      (bst/remove value)
      balance))

(def member?    bst/member?)
(def min-val    bst/min-val)
(def max-val    bst/max-val)
(def tree->list bst/tree->list)
(def size       bst/size)

(defn make
  "Creates an AVL tree. With no arguments, defaults to < as the ordering
  function and number? as the value predicate. Accepts an optional collection
  of initial values which are inserted and balanced in sequence."
  ([]                   (make < number? [] default-node-factory))
  ([vals]               (make < number? vals))
  ([to-left? pred]      (make to-left? pred [] default-node-factory))
  ([to-left? pred vals] (make to-left? pred vals default-node-factory))
  ([to-left? pred
    vals node-factory]  (reduce insert
                                {:to-left? to-left? :pred pred :node-factory node-factory :root nil}
                                (seq vals))))
