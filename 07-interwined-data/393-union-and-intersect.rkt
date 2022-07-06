;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 393-union-and-intersect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son (set of numbers) is one of:
; - '()
; - (cons Number Son)
; Constraint: no number occurs twice in Son

; Son Son -> Son
; produces a set that contains the elements of s1 and s2
(check-expect (union '() '()) '())
(check-expect (union '() '(1)) '(1))
(check-expect (union '(1) '()) '(1))
(check-expect (union '(1) '(2)) '(1 2))
(check-expect (union '(1) '(1)) '(1))
(check-expect (union '(1 2 3) '(3 4 5 6)) '(1 2 3 4 5 6))
(check-expect (union '(3 2 1) '(1 2 3)) '(1 2 3))
(check-expect (union '(1 10 3) '(3 4 10 6)) '(1 3 4 10 6))
(check-expect (union '(100 10) '(0 5 6 3.3)) '(100 10 0 5 6 3.3))

(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [else (if (member? (first s1) s2)
              (union (rest s1) s2)
              (cons (first s1) (union (rest s1) s2)))]))

; Son Son -> Son
; produces the set of exactly those elements that occur in both s1 and s2
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '(1)) '(1))
(check-expect (intersect '(1) '(2)) '())
(check-expect (intersect '(1 2 3) '(1 2 3)) '(1 2 3))
(check-expect (intersect '(1 99 3) '(99 2.2 1)) '(1 99))
(check-expect (intersect '(1 99 2 3 5) '(0 1 2 3 4)) '(1 2 3))

(define (intersect s1 s2)
  (cond
    [(empty? s1) '()]
    [else (if (member? (first s1) s2)
              (cons (first s1) (intersect (rest s1) s2))
              (intersect (rest s1) s2))]))
