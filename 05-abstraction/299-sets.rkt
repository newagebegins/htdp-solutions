;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 299-sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a function:
;   [X -> Boolean]
; interpretation: if s is a Set and x is an X then
; (s x) produces #true if x belongs to s, #false otherwise

(define EMPTY (lambda (x) #false))
(define ODD-NUMBERS odd?)
(define EVEN-NUMBERS even?)
(define SET123 (lambda (x) (or (= x 1) (= x 2) (= x 3))))
(define SET234 (lambda (x) (or (= x 2) (= x 3) (= x 4))))

; X Set -> Boolean
; returns #true if x is in s, #false otherwise
(check-expect (in-set? 3 EMPTY) #false)
(check-expect (in-set? 3 ODD-NUMBERS) #true)
(check-expect (in-set? 2 ODD-NUMBERS) #false)

(define (in-set? x s)
  (s x))

; X Set -> Set
; add x0 to the set s
(check-expect (in-set? 3 (add-element 3 EMPTY)) #true)
(check-expect (in-set? 2 (add-element 2 ODD-NUMBERS)) #true)
(check-expect (in-set? 5 (add-element 2 ODD-NUMBERS)) #true)
(check-expect (in-set? 4 (add-element 2 ODD-NUMBERS)) #false)

(define (add-element x0 s)
  (lambda (x)
    (or (equal? x x0)
        (s x))))

; Set Set -> Set
; combine the elemenets of two sets
(check-expect (in-set? 1 (union EVEN-NUMBERS ODD-NUMBERS)) #true)
(check-expect (in-set? 2 (union EVEN-NUMBERS ODD-NUMBERS)) #true)

(define (union s1 s2)
  (lambda (x)
    (or (s1 x)
        (s2 x))))

; Set Set -> Set
; collects all elements common to two sets
(check-expect (in-set? 1 (intersect EVEN-NUMBERS ODD-NUMBERS)) #false)
(check-expect (in-set? 2 (intersect EVEN-NUMBERS ODD-NUMBERS)) #false)
(check-expect (in-set? 1 (intersect SET123 SET234)) #false)
(check-expect (in-set? 2 (intersect SET123 SET234)) #true)
(check-expect (in-set? 3 (intersect SET123 SET234)) #true)
(check-expect (in-set? 4 (intersect SET123 SET234)) #false)

(define (intersect s1 s2)
  (lambda (x)
    (and (s1 x)
         (s2 x))))
