;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 326-create-bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; BST Number Symbol -> BST
; add a new node to the bst with the given ssn and name

; NONE -> 1
(check-expect (create-bst NONE 1 'a) (make-node 1 'a NONE NONE))

; 10   5->  10
;           /
;          5
(check-expect (create-bst (make-node 10 'a NONE NONE) 5 'b)
              (make-node 10 'a (make-node 5 'b NONE NONE) NONE))

; 10   15->   10
;               \
;                15
(check-expect (create-bst (make-node 10 'a NONE NONE) 15 'b)
              (make-node 10 'a NONE (make-node 15 'b NONE NONE)))

;    20    10->     20
;   /  \           /  \
;  8   30         8    30
;                  \
;                   10
(check-expect (create-bst (make-node 20 'a
                                     (make-node 8 'b NONE NONE)
                                     (make-node 30 'c NONE NONE))
                          10 'd)
              (make-node 20 'a
                         (make-node 8 'b
                                    NONE
                                    (make-node 10 'd NONE NONE))
                         (make-node 30 'c NONE NONE)))

;    20    7->      20
;   /  \           /  \
;  8   30         8    30
;                /
;               7
(check-expect (create-bst (make-node 20 'a
                                     (make-node 8 'b NONE NONE)
                                     (make-node 30 'c NONE NONE))
                          7 'd)
              (make-node 20 'a
                         (make-node 8 'b
                                    (make-node 7 'd NONE NONE)
                                    NONE)
                         (make-node 30 'c NONE NONE)))

;    20    25->      20
;   /  \            /  \
;  8   30          8    30
;                       /
;                     25
(check-expect (create-bst (make-node 20 'a
                                     (make-node 8 'b NONE NONE)
                                     (make-node 30 'c NONE NONE))
                          25 'd)
              (make-node 20 'a
                         (make-node 8 'b NONE NONE)
                         (make-node 30 'c
                                    (make-node 25 'd NONE NONE)
                                    NONE)))

;    20    35->      20
;   /  \            /  \
;  8   30          8    30
;                         \
;                          35
(check-expect (create-bst (make-node 20 'a
                                     (make-node 8 'b NONE NONE)
                                     (make-node 30 'c NONE NONE))
                          35 'd)
              (make-node 20 'a
                         (make-node 8 'b NONE NONE)
                         (make-node 30 'c
                                    NONE
                                    (make-node 35 'd NONE NONE))))

(check-error (create-bst (make-node 1 'a NONE NONE) 1 'b) "the tree already has a node with ssn 1")

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
