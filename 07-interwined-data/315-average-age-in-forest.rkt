;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 315-average-age-in-forest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of:
; - NP
; - (make-child FT FT String N String)

; An FF (short for family forest) is a [List-of FT]
; interpretation: a family forest represents several families (say, a town) and their ancestor trees

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

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

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

; FF N -> N
; sum the ages of all child instances in the forest
(check-expect (sum-ages-in-forest ff1 1930) (+ 4 4))
(check-expect (sum-ages-in-forest ff2 2000) (+ 34 35 74 74))

(define (sum-ages-in-forest forest year)
  (for/sum ([tree forest])
    (sum-ages tree year)))

; FF -> N
; count all child instances in the forest
(check-expect (count-persons-in-forest ff1) 2)
(check-expect (count-persons-in-forest ff2) 4)

(define (count-persons-in-forest forest)
  (for/sum ([tree forest])
    (count-persons tree)))

; FF N -> Number
; calculate the average age of all child instances in the forest
(check-expect (average-age ff1 1930) (/ (+ 4 4) 2))
(check-expect (average-age ff2 2000) (/ (+ 34 35 74 74) 4))

(define (average-age forest year)
  (/ (sum-ages-in-forest forest year)
     (count-persons-in-forest forest)))
