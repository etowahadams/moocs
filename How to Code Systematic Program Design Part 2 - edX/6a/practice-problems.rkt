; P2


;; sum-keys-starter.rkt

;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST


;; Functions

; BST -> Number
; consumes a BST and produces the sum of all the keys in the BST.
(check-expect (total-key BST0) 0)
(check-expect (total-key BST1) 1)
(check-expect (total-key BST42) 83)

;(define (total-key t) 0)  ;stub

(define (total-key t)
  (cond [(false? t) 0]
        [else
         (+ (node-key t)
            (total-key (node-l t))
            (total-key (node-r t)))]))


; P4


;; insert-starter.rkt

;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

; .

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Integer String BST -> BST
;; adds a node that has the given key and value to the tree,
;; assuming it does not already exist
(check-expect (insert 6 "soc" false) (make-node 6 "soc" false false))
(check-expect (insert 3 "ilk" (make-node 10 "why" false false)) (make-node 10 "why" (make-node 3 "ilk" false false) false))
(check-expect (insert 26 "mnb" BST42) (make-node 42 "ily"
                                                 (make-node 27 "wit" (make-node 14 "olp" false (make-node 26 "mnb" false false)) false)
                                                 false))

;(define (insert i s t) false)   ;stub
;<template from BST>
(define (insert k v bst)
  (cond [(false? bst) (make-node k v false false)]
        [else
         (if (< k (node-key bst))
             (make-node (node-key bst) 
                        (node-val bst) 
                        (insert k v (node-l bst))
                        (node-r bst))
             (make-node (node-key bst) 
                        (node-val bst) 
                        (node-l bst)
                        (insert k v (node-r bst))))]))





; P6



;; render-bst-w-lines-starter.rkt

(require 2htdp/image)

;; Constants

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 100 1 "solid" "white"))
(define LINE-BG (rectangle 200 30 "solid" "white"))
(define LINE (add-line (add-line (rectangle 200 30 "solid" "white") 50 30 100 0 "black") 100 0 150 30 "black"))


;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree
; .

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))
#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

;; Functions:
;; BST -> Image
;; produces a SIMPLE rendering of that bst including lines between nodes and their subnodes.
(check-expect (render false) MTTREE)
(check-expect (render (make-node 10 "tns" false false))
              (above
               (key-value 10 "tns")
               LINE
               (beside MTTREE MTTREE)))
(check-expect (render (make-node 20 "adf" (make-node 10 "bcs" false false) false))
              (above
               (text (string-append "20" KEY-VAL-SEPARATOR "adf") TEXT-SIZE TEXT-COLOR)
               LINE
               (beside (above (key-value 10 "bcs") LINE (beside MTTREE MTTREE))
                       MTTREE)))
;(define (render bst) MTTREE)   ;stub
;<template from BST>

(define (render t)
  (cond [(false? t) MTTREE]
        [else
         (above (key-value (node-key t) (node-val t))  
                LINE   
                (beside (render (node-l t))
                        (render (node-r t))))]))

;; Number String -> Image
;; creates a key value image

(check-expect (key-value 10 "tns") (text (string-append "10" KEY-VAL-SEPARATOR "tns") TEXT-SIZE TEXT-COLOR))

;(define (key-value n s) empty-image)  ;stub

(define (key-value n s)
  (text (string-append (number->string n) KEY-VAL-SEPARATOR s)
        TEXT-SIZE
        TEXT-COLOR))
