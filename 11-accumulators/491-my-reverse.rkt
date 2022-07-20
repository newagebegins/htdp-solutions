;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 491-my-reverse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [List-of X]
(check-expect (add-at-end 1 '(4 3 2)) '(4 3 2 1))

(define (add-at-end x l)
  (cond
    [(empty? l) (list x)]
    [else (cons (first l) (add-at-end x (rest l)))]))

; List -> List
(check-expect (my-reverse '(1 2 3 4)) '(4 3 2 1))

(define (my-reverse l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (first l) (my-reverse (rest l)))]))

; my-reverse takes on the order of N^2 steps for a list with N items
