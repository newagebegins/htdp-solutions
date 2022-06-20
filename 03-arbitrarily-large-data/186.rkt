;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |186|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [(cons? (rest l)) (and (> (first l) (first (rest l)))
                           (sorted>? (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))
(check-expect (insert 20 (list 20 -5)) (list 20 20 -5))
(check-expect (insert 4 (list 3 2 1)) (list 4 3 2 1))
(check-expect (insert 0 (list 3 2 1)) (list 3 2 1 0))

(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (< n (first alon))
              (cons (first alon) (insert n (rest alon)))
              (cons n alon))]))

; List-of-numbers -> List-of-numbers
; rearranges alon in descending order

(check-expect (sort> '()) '())
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (sort> (rest alon)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-satisfied (sort>/bad (list 1 2 3)) sorted>?)
(check-expect (sort>/bad (list 1 2 3)) (list 3 2 1))

(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))
