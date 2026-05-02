# clj-data-structures

A study progression toward implementing **HAMTs** and **B-Trees** in Clojure.

The approach is incremental: each data structure builds intuition and vocabulary for the next. Rather than jumping straight to the target structures, the path goes through BSTs, AVL trees, and other foundational shapes first.

## Roadmap

- [x] Binary Search Tree (BST)
- [ ] AVL Tree
- [ ] Red-Black Tree
- [ ] Trie
- [ ] B-Tree
- [ ] HAMT

---

## Binary Search Tree

A BST is a binary tree where every node satisfies the invariant: all values in the left subtree come before the node's value, and all values in the right subtree come after it — as defined by a comparator function `to-left?`.

The implementation is purely functional: every operation returns a new tree, leaving the original unchanged.

```clojure
(require '[clj-data-structures.bst :as bst])

(def by-length #(< (count %1) (count %2)))

; initialize from a collection
(def t (bst/make-tree by-length string? ["hi" "hello" "hey" "howdy" "x"]))

; or build incrementally with add
(def t (-> (bst/make-tree by-length string?)
           (bst/add "hi")
           (bst/add "hello")
           (bst/add "hey")
           (bst/add "howdy")
           (bst/add "x")))

(bst/to-seq t)         ;; => ("x" "hi" "hey" "hello" "howdy")
(bst/min-val t)        ;; => "x"
(bst/max-val t)        ;; => "howdy"
(bst/member? t "hey")  ;; => true
(bst/member? t "bye")  ;; => false

(bst/to-seq (bst/delete t "hi"))  ;; => ("x" "hey" "hello" "howdy")
```

Values are ordered by string length, shorter strings go left.
