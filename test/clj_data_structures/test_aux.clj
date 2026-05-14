(ns clj-data-structures.test-aux)

(defn- avl-balanced-node? [{:keys [left right factor] :as node}]
  (if (nil? node)
    true
    (and (< -2 factor 2)
         (avl-balanced-node? left)
         (avl-balanced-node? right))))

(defn avl-balanced? [{:keys [root]}]
  (if (nil? root)
    true
    (avl-balanced-node? root)))

(defn- black-height [node]
  (if (nil? node)
    0
    (let [lh (black-height (:left node))
          rh (black-height (:right node))]
      (when (and lh rh (= lh rh))
        (+ lh (if (= (:colour node) :black) 1 0))))))

(defn- no-red-red? [node]
  (if (nil? node)
    true
    (and (or (not= (:colour node) :red)
             (not= (:colour (:left node))  :red))
         (or (not= (:colour node) :red)
             (not= (:colour (:right node)) :red))
         (no-red-red? (:left node))
         (no-red-red? (:right node)))))

(defn brt-valid? [{:keys [root]}]
  (and (or (nil? root) (= (:colour root) :black))
       (no-red-red? root)
       (some? (black-height root))))
