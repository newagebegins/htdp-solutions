;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 324-inorder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

; A sample binary tree:
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

; BT -> [List-of Number]
; produce the sequence of all the ssn numbers in the tree as the show up
; from left to right when looking at a tree drawing
(check-expect (inorder NONE) '())
(check-expect (inorder N10) '(10))
(check-expect (inorder N15) '(10 15 24))
(check-expect (inorder N29) '(10 15 24 29))
(check-expect (inorder N89) '(77 89 95 99))
(check-expect (inorder N63) '(10 15 24 29 63 77 89 95 99))

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt))
                  (list (node-ssn bt))
                  (inorder (node-right bt)))]))
