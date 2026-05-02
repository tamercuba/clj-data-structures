(ns clj-data-structures.bst)

(defn make-node
  "Creates a BST node map with the given value and optional left/right children.
  With one argument, children default to nil."
  ([value] (make-node value nil nil))
  ([value l r] {:value value :left l :right r}))

(defn- min-node [{:keys [left] :as node}]
  (if (nil? left)
    node
    (recur left)))

(defn- max-node [{:keys [right] :as node}]
  (if (nil? right)
    node
    (recur right)))

(defn- node-add [{:keys [value left right] :as node} cur-value to-left?]
  (if (to-left? cur-value value)
    (if (nil? left)  (assoc node :left  (make-node cur-value)) (assoc node :left  (node-add left  cur-value to-left?)))
    (if (nil? right) (assoc node :right (make-node cur-value)) (assoc node :right (node-add right cur-value to-left?)))))

(defn- node-member? [{:keys [value left right] :as node} search-value to-left?]
  (cond
    (nil? node)                     false
    (= value search-value)          true
    (to-left? search-value value)   (if left  (node-member? left  search-value to-left?) false)
    :else                           (if right (node-member? right search-value to-left?) false)))

(defn- node-delete [{:keys [value left right] :as node} delete-value to-left?]
  (cond
    (to-left? delete-value value) (update node :left  node-delete delete-value to-left?)
    (to-left? value delete-value) (update node :right node-delete delete-value to-left?)
    :else (cond
            (nil? left)  right
            (nil? right) left
            :else
            (let [successor (min-node right)]
              (-> node
                  (assoc  :value (:value successor))
                  (update :right node-delete (:value successor) to-left?))))))

(defn- node-to-seq [{:keys [value left right] :as node}]
  (when node
    (concat (node-to-seq left)
            [value]
            (node-to-seq right))))

(defn add
  "Inserts value into tree. Returns a new tree with value inserted at the
  correct position according to to-left?. Throws ex-info if value does not
  satisfy the tree's pred."
  [{:keys [to-left? pred root] :as tree} value]
  (if (pred value)
    (if (nil? root)
      (assoc tree :root (make-node value))
      (assoc tree :root (node-add root value to-left?)))
    (throw (ex-info (str value " doesnt satisfy the pred" pred) {:key value}))))

(defn member?
  "Returns true if search-value is present in tree, false otherwise.
  Throws ex-info if search-value does not satisfy the tree's pred."
  [{:keys [root to-left? pred]} search-value]
  (if (pred search-value)
    (node-member? root search-value to-left?)
    (throw (ex-info (str search-value " doesnt satisfy the pred" pred) {:key search-value}))))

(defn min-val
  "Returns the minimum value in tree, or nil if the tree is empty."
  [{:keys [root]}]
  (if (some? root)
    (:value (min-node root))
    nil))

(defn max-val
  "Returns the maximum value in tree, or nil if the tree is empty."
  [{:keys [root]}]
  (if (some? root)
    (:value (max-node root))
    nil))

(defn to-seq
  "Returns a lazy sequence of the tree's values in ascending order (in-order
  traversal). Returns nil if the tree is empty."
  [{:keys [root]}]
  (node-to-seq root))

(defn delete
  "Removes value from tree. Returns a new tree with value removed. Throws
  ex-info if value does not satisfy the tree's pred."
  [{:keys [to-left? pred root] :as tree} value]
  (if (pred value)
    (assoc tree :root (node-delete root value to-left?))
    (throw (ex-info (str value " doesnt satisfy the pred" pred) {:key value}))))

(defn make-tree
  "Creates an empty BST. With no arguments, defaults to < as the ordering
  function and number? as the value predicate."
  ([] (make-tree < number?))
  ([vals] (make-tree < number? vals))
  ([to-left? pred] {:to-left? to-left? :pred pred :root nil})
  ([to-left? pred vals]
   (reduce add (make-tree to-left? pred) (seq vals))))

