;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |139|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A PositiveNumber is a Number greater than/equal to 0.

; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)

; List-of-amounts -> PositiveNumber
; compute sum of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 2 '())) 2)
(check-expect (sum (cons 1 (cons 2 '()))) 3)
(check-expect (sum (cons 1 (cons 0 (cons 5.1 '())))) 6.1)

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa) (+ (first loa) (sum (rest loa)))]))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; determine whether all numbers in the list are positive
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 1.5 '())) #true)
(check-expect (pos? (cons 0 '())) #true)
(check-expect (pos? (cons -4 '())) #false)
(check-expect (pos? (cons 5 (cons -4 '()))) #false)

(define (pos? l)
  (cond
    [(empty? l) #true]
    [(cons? l) (and (>= (first l) 0) (pos? (rest l)))]))

; List-of-numbers -> Number
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 5.5 '())) 5.5)
(check-expect (checked-sum (cons 3 (cons 5.5 '()))) 8.5)
(check-error (checked-sum (cons -1 (cons 5.5 '()))) "only positive numbers expected")

(define (checked-sum l)
  (if (pos? l)
      (sum l)
      (error "only positive numbers expected")))
