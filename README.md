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

A BST is a binary tree where every node satisfies the invariant: all values in the left subtree come before the node's value, and all values in the right subtree come after it, as defined by a comparator function `to-left?`.

The implementation is purely functional: every operation returns a new tree, leaving the original unchanged.

```clojure
(require '[clj-data-structures.bst :as bst])

;; How I want to compare which el goes to left and which goes to right?
;; Default: <
(def by-length #(< (count %1) (count %2)))

;; `string` here means: Is the value inserted a value/desired one?
;; Default: number?
(def t (bst/make-tree by-length string? ["hi" "hello" "hey" "howdy" "x"]))

(def t (-> (bst/make-tree by-length string?)
           (bst/insert "hi")
           (bst/insert "hello")
           (bst/insert "hey")
           (bst/insert "howdy")
           (bst/insert "x")))

(bst/tree->list t)     ;; => ("x" "hi" "hey" "hello" "howdy")
(bst/min-val t)        ;; => "x"
(bst/max-val t)        ;; => "howdy"
(bst/member? t "hey")  ;; => true
(bst/member? t "bye")  ;; => false

(bst/tree->list (bst/remove t "hi"))  ;; => ("x" "hey" "hello" "howdy")
```

Values are ordered by string length, shorter strings go left.
