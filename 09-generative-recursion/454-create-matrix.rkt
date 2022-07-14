;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 454-create-matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N [List-of Number] -> [List-of Number]
; create a list from the first n numbers of l
(check-expect (row 0 '()) '())
(check-expect (row 1 '(1)) '(1))
(check-expect (row 2 '(1 2 3 4)) '(1 2))
(check-expect (row 3 '(1 2 3 4 5 6 7 8 9)) '(1 2 3))
(check-expect (row 4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) '(1 2 3 4))

(define (row n l)
  (cond
    [(zero? n) '()]
    [else (cons (first l) (row (sub1 n) (rest l)))]))

; N [List-of Number] -> [List-of [List-of Number]]
; remove the first n numbers of l
(check-expect (remove-row 0 '()) '())
(check-expect (remove-row 0 '(1 2 3)) '(1 2 3))
(check-expect (remove-row 1 '(1)) '())
(check-expect (remove-row 2 '(1 2 3 4)) '(3 4))
(check-expect (remove-row 3 '(1 2 3 4 5 6 7 8 9)) '(4 5 6 7 8 9))
(check-expect (remove-row 4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) '(5 6 7 8 9 10 11 12 13 14 15 16))

(define (remove-row n l)
  (cond
    [(zero? n) l]
    [else (remove-row (sub1 n) (rest l))]))

; N [List-of Number] -> [List-of [List-of Number]]
; produces an n by n matrix from the numbers in l
; assume: l contains n^2 numbers
(check-expect (create-matrix 0 '())
              '())
(check-expect (create-matrix 1 '(1))
              '((1)))
(check-expect (create-matrix 2 '(1 2 3 4))
              '((1 2)
                (3 4)))
(check-expect (create-matrix 3 '(1 2 3 4 5 6 7 8 9))
              '((1 2 3)
                (4 5 6)
                (7 8 9)))
(check-expect (create-matrix 4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
              '((1 2 3 4)
                (5 6 7 8)
                (9 10 11 12)
                (13 14 15 16)))

(define (create-matrix n l)
  (cond
    [(empty? l) '()]
    [else (cons (row n l) (create-matrix n (remove-row n l)))]))
