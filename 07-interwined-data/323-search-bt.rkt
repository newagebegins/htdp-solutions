;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 323-search-bt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

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

; Number BT -> [Maybe Symbol]
; if bt contains a node whose ssn is n, return the name of that node
; otherwise return #false
(check-expect (search-bt 24 NONE) #false)
(check-expect (search-bt 24 N24) 'b)
(check-expect (search-bt 3 N24) #false)
(check-expect (search-bt 77 N63) 'e)

(define (search-bt n bt)
  (cond
    [(no-info? bt) #false]
    [else (local ((define s1 (if (= (node-ssn bt) n) (node-name bt) #false))
                  (define s2 (search-bt n (node-left bt)))
                  (define s3 (search-bt n (node-right bt))))
            (for/or ([x (list s1 s2 s3)])
              x))]))
