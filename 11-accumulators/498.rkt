;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |498|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node [left right])
; A Tree is one of:
; – '()
; – (make-node Tree Tree)

; Tree -> N
; compute the height of a binary tree
(check-expect (height.v3 '()) 0)
(check-expect (height.v3 (make-node '() '())) 1)
(check-expect (height.v3 (make-node
                          (make-node '()
                                     (make-node '() '()))
                          '())) 3)

(define (height.v3 abt0)
  (local (; Tree N N -> N
          ; measures the height of abt
          ; accumulator: s is the number of steps it takes to reach abt from abt0
          ; accumulator: m is the maximal height of the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) (max s m)]
              [else
               (local ((define left-height (h/a (node-left abt) (add1 s) m)))
                 (h/a (node-right abt) (add1 s) left-height))])))
    (h/a abt0 0 0)))
