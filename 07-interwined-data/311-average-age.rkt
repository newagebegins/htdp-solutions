;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 311-average-age) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of:
; - NP
; - (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> N
; count the child structures in the family tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)

(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [else (+ 1
             (count-persons (child-father ft))
             (count-persons (child-mother ft)))]))

; FT N -> N
; given a family tree and the current year, sum the ages of all the persons in the family tree
(check-expect (sum-ages NP 1927) 0)
(check-expect (sum-ages Carl 1927) 1)
(check-expect (sum-ages Bettina 1930) 4)
(check-expect (sum-ages Adam 1966) (+ 16 40 40))
(check-expect (sum-ages Gustav 2000) (+ 12 34 35 74 74))

(define (sum-ages ft cy)
  (cond
    [(no-parent? ft) 0]
    [else (+ (sum-ages (child-father ft) cy)
             (sum-ages (child-mother ft) cy)
             (- cy (child-date ft)))]))

; FT N -> Number
; given a family tree and the current year, compute the average age of all child structures in tree
(check-expect (average-age Carl 1927) 1)
(check-expect (average-age Bettina 1930) 4)
(check-expect (average-age Adam 1966) (/ (+ 16 40 40) 3))
(check-expect (average-age Gustav 2000) (/ (+ 12 34 35 74 74) 5))

(define (average-age ft cy)
  (/ (sum-ages ft cy) (count-persons ft)))
