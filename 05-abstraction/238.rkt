;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |238|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define L2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf '(5 2 9)) 2)

(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup '(2 9 5)) 9)

(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

(check-expect (extract > '(2 9 5)) 9)
(check-expect (extract < '(5 2 9)) 2)

(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (extract R (rest l)))
         (first l)
         (extract R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-1 '(5 2 9)) 2)

(define (inf-1 l)
  (extract < l))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-1 '(2 9 5)) 9)

(define (sup-1 l)
  (extract > l))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-min '(5 2 9)) 2)

(define (inf-min l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (min (first l) (inf-min (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-max '(2 9 5)) 9)

(define (sup-max l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (max (first l) (sup-max (rest l)))]))

(check-expect (extract-2 min '(5 2 9)) 2)
(check-expect (extract-2 max '(2 9 5)) 9)

(define (extract-2 R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else (R (first l) (extract-2 R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-2 '(5 2 9)) 2)

(define (inf-2 l)
  (extract-2 min l))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-2 '(2 9 5)) 9)

(define (sup-2 l)
  (extract-2 max l))
