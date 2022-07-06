;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 394-merge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number [List-of Number] -> [List-of Number]
; insert a number n into a sorted (ascending) list of numbers l, so that
; the output is also sorted
(check-expect (insert 1 '()) '(1))
(check-expect (insert 1 '(2)) '(1 2))
(check-expect (insert 2 '(1)) '(1 2))
(check-expect (insert 1 '(1 2)) '(1 1 2))
(check-expect (insert 25 '(10 20 30)) '(10 20 25 30))

(define (insert n l)
  (cond
    [(empty? l) (list n)]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; [List-of Number] [List-of Number] -> [List-of Number]
; produce a sorted list of numbers that contains all the numbers from l1 and l2
; l1 and l2 are sorted in ascending order
; a number occurs in the output as many times as it occurs on the two input lists together
(check-expect (merge '() '()) '())
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '() '(1)) '(1))
(check-expect (merge '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (merge '(10 20 30) '(15 25 35 40)) '(10 15 20 25 30 35 40))
(check-expect (merge '(1 2 3) '(1 2 3)) '(1 1 2 2 3 3))
(check-expect (merge '(1 2 3 4) '(1 2 2.5 3)) '(1 1 2 2 2.5 3 3 4))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [else (insert (first l1) (merge (rest l1) l2))]))
