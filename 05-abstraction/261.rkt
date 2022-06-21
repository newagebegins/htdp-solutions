;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |261|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of:
; – '()
; – (cons IR Inventory)

(define IR1 (make-IR "IR1" 0.4))
(define IR2 (make-IR "IR2" 0.9))
(define IR3 (make-IR "IR3" 1.0))
(define IR4 (make-IR "IR4" 1.2))
(define IR5 (make-IR "IR5" 1.8))

; Inventory -> Inventory
; creates an Inventory from an-inv for all those items
; that cost less than a dollar
(check-expect (extract1 '()) '())
(check-expect (extract1 (list IR1 IR2 IR3 IR4 IR5)) (list IR1 IR2 IR3))
(check-expect (extract1 (list IR4 IR5)) '())
(check-expect (extract1 (list IR4 IR2 IR1 IR5 IR3)) (list IR2 IR1 IR3))

(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (local ((define rest1 (extract1 (rest an-inv))))
            (cond
              [(<= (IR-price (first an-inv)) 1.0)
               (cons (first an-inv) rest1)]
              [else rest1]))]))

; in this case local does not improve performance because
; even without local (extract1 (rest an-inv)) is executed only once
