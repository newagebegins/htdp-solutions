;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 322-contains-bt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Number BT -> Boolean
; return #true there is a node with the given ssn in bt
(check-expect (contains-bt? 10 NONE) #false)
(check-expect (contains-bt? 10 N10) #true)
(check-expect (contains-bt? 24 N24) #true)
(check-expect (contains-bt? 10 N24) #false)

(check-expect (contains-bt? 15 N15) #true)
(check-expect (contains-bt? 10 N15) #true)
(check-expect (contains-bt? 24 N15) #true)
(check-expect (contains-bt? 23 N15) #false)

(check-expect (contains-bt? 99 N63) #true)
(check-expect (contains-bt? 24 N63) #true)
(check-expect (contains-bt? 77 N63) #true)
(check-expect (contains-bt? 15 N63) #true)
(check-expect (contains-bt? 7 N63) #false)

(define (contains-bt? ssn bt)
  (cond
    [(no-info? bt) #false]
    [else (or (= (node-ssn bt) ssn)
              (contains-bt? ssn (node-left bt))
              (contains-bt? ssn (node-right bt)))]))