(ns clj-data-structures.avl
  (:require [clj-data-structures.bst :as bst]))

(defn- node-height [node]
  (if (nil? node)
    0
    (+ 1 (max (node-height (:left node))
              (node-height (:right node))))))

(defn- node-factor [node]
  (if (nil? node)
    0
    (- (node-height (:left node)) (node-height (:right node)))))

(defn- balanced-node? [{:keys [left right] :as node}]
  (if (nil? node)
    true
    (and (< -2 (node-factor node) 2)
         (balanced-node? left)
         (balanced-node? right))))

(defn- rotate-left [{:keys [right] :as node}]
  (-> right
      (assoc :left (assoc node :right (:left right)))))

(defn- rotate-right [{:keys [left] :as node}]
  (-> left
      (assoc :right (assoc node :left (:right left)))))

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
                  (assoc :right (when right (balance-nodes right))))
        left  (:left node)
        right (:right node)
        nf    (node-factor node)]
    (cond
      (and (> nf 1)  (>= (node-factor left)  0)) (rotate-right node)
      (> nf 1)                                    (rotate-left-right node)
      (and (< nf -1) (<= (node-factor right) 0)) (rotate-left node)
      (< nf -1)                                   (rotate-right-left node)
      :else                                        node)))

(defn height [{:keys [root]}]
  (if (nil? root)
    0
    (node-height root)))

(defn balanced? [{:keys [root]}]
  (if (nil? root)
    true
    (balanced-node? root)))

(defn balance [{:keys [root] :as tree}]
  (if (nil? root) tree (assoc tree :root (balance-nodes root))))

(defn insert [tree value]
  (-> tree
      (bst/insert value)
      balance))

(defn make
  ([] (bst/make))
  ([vals] (make < number? vals))
  ([to-left? pred] (bst/make to-left? pred))
  ([to-left? pred vals]
   (reduce insert (make to-left? pred) (seq vals))))

(comment
  (let [t (make '(5 4 3 10 9 11))]
    (println  (node-height (:root t)))
    (println  (height t))))
