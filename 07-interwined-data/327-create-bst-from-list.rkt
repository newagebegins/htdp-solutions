;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 327-create-bst-from-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

; A BST (short for binary search tree) is a BT according to the following conditions:
; - NONE is always a BST.
; - (make-node ssn0 name0 L R) is a BST if
;   - L is a BST,
;   - R is a BST,
;   - all ssn fields in L are smaller than ssn0,
;   - all ssn fields in R are larger than ssn0.

; A sample binary search tree:
;                63
;               /  \
;             29    89
;             /     / \
;            15    77  95
;           /  \        \
;          10  24        99
(define N10 (make-node 10 'a NONE NONE))
(define N24 (make-node 24 'b NONE NONE))
(define N99 (make-node 99 'c NONE NONE))

(define N15 (make-node 15 'd N10 N24))
(define N77 (make-node 77 'e NONE NONE))
(define N95 (make-node 95 'f NONE N99))

(define N29 (make-node 29 'g N15 NONE))
(define N89 (make-node 89 'h N77 N95))

(define N63 (make-node 63 'i N29 N89))

; BST Number Symbol -> BST
; add a new node to the bst with the given ssn and name
(define (create-bst bst ssn name)
  (cond
    [(no-info? bst) (make-node ssn name NONE NONE)]
    [else ; a node
     (cond
       [(= ssn (node-ssn bst)) (error "the tree already has a node with ssn " ssn)]
       [(< ssn (node-ssn bst)) (make-node (node-ssn bst)
                                          (node-name bst)
                                          (create-bst (node-left bst) ssn name)
                                          (node-right bst))]
       [(> ssn (node-ssn bst)) (make-node (node-ssn bst)
                                          (node-name bst)
                                          (node-left bst)
                                          (create-bst (node-right bst) ssn name))])]))

; [List-of [List Number Symbol]] -> BST
; create a BST from the given list of numbers and names
(check-expect (create-bst-from-list '()) NONE)
(check-expect (create-bst-from-list '((1 a))) (make-node 1 'a NONE NONE))
(check-expect (create-bst-from-list '((1 a) (2 b))) (make-node 2 'b (make-node 1 'a NONE NONE) NONE))
(check-expect (create-bst-from-list '((2 b) (1 a))) (make-node 1 'a NONE (make-node 2 'b NONE NONE)))
(check-expect (create-bst-from-list '((99 c) (77 e) (24 b) (10 a) (95 f) (15 d) (89 h) (29 g) (63 i))) N63)

(define (create-bst-from-list l)
  (foldr (lambda (pair bst)
           (create-bst bst (first pair) (second pair)))
         NONE
         l))
