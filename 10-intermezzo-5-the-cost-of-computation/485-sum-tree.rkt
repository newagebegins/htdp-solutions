;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 485-sum-tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NT is one of:
; - Number
; - (list NT NT)
; interpretation: a number tree

; NT -> Number
; determines the sum of the numbers in a tree
(check-expect (sum-tree 5) 5)
(check-expect (sum-tree (list 2 8)) 10)
(check-expect (sum-tree (list (list 8 2) (list 3 4))) 17)
(check-expect (sum-tree (list (list (list 9 9) 2)
                              (list 3 4))) 27)
(check-expect (sum-tree (list (list (list 9 9) 2)
                              (list 3 (list 1 (list 4 5))))) 33)

(define (sum-tree t)
  (cond
    [(number? t) t]
    [else (+ (sum-tree (first t))
             (sum-tree (second t)))]))

; sum-tree takes on the order of n steps to complete
; n - number of nodes in a tree
; we must visit every node to calculate the sum
; there is no worst or best case
