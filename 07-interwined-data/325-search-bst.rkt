;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 325-search-bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A SymbolOrNone is one of:
; - Symbol
; - NONE

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

; Number BST -> SymbolOrNone
; if bst contains a node whose ssn is n, return the name of that node
; otherwise return NONE
(check-expect (search-bst 10 NONE) NONE)
(check-expect (search-bst 10 N10) 'a)
(check-expect (search-bst 95 N63) 'f)
(check-expect (search-bst 100 N63) NONE)
(check-expect (search-bst 24 N63) 'b)
(check-expect (search-bst 99 N63) 'c)

(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [(= n (node-ssn bst)) (node-name bst)]
    [(< n (node-ssn bst)) (search-bst n (node-left bst))]
    [else (search-bst n (node-right bst))]))
