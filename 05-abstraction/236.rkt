;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |236|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* '(1 2 3)) '(2 3 4))

(define (add1* l)
  (add* 1 l))

; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 '(1 2 3)) '(6 7 8))

(define (plus5 l)
  (add* 5 l))

; Lon -> Lon
; adds x to each item on l
(check-expect (add* 1 '(1 2 3)) '(2 3 4))
(check-expect (add* 5 '(1 2 3)) '(6 7 8))

(define (add* x l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) x)
       (add* x (rest l)))]))

; Lon -> Lon
; subtracts 2 from each item on l
(check-expect (sub2 '(1 2 3)) '(-1 0 1))

(define (sub2 l)
  (add* -2 l))
