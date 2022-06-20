;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |147|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NEList-of-booleans if one of:
; - (cons Boolean '())
; - (cons Boolean List-of-booleans)

; NEList-of-booleans -> Boolean
; determine whether all values in the list are true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)

(define (all-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (and (first l) (all-true (rest l)))]))

; NEList-of-booleans -> Boolean
; determine whether at least one item on the list is true
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)

(define (one-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (or (first l) (one-true (rest l)))]))
