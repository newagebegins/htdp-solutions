;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |268|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct IR [name description acq-price sale-price])
; An IR is a structure:
;   (make-IR String String Number Number)
; interp. An inventory record with name, description, acquisition price and recommended sales price

(define IR1 (make-IR "A" "AA" 1 2))
(define IR2 (make-IR "B" "BB" 2 4))
(define IR3 (make-IR "C" "CC" 0.5 3.5))

; [List-of IR] -> [List-of IR]
; sort inventory records by the difference between the two prices
(check-expect (sort-by-price-diff '()) '())
(check-expect (sort-by-price-diff (list IR3 IR1 IR2)) (list IR1 IR2 IR3))

(define (sort-by-price-diff l)
  (local (; IR IR -> Boolean
          (define (cmp r1 r2)
            (local (; IR -> Number
                    (define (price-diff r)
                      (- (IR-sale-price r) (IR-acq-price r))))
              (< (price-diff r1) (price-diff r2)))))
    (sort l cmp)))
