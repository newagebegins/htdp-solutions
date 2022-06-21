;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |272|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; List List -> List
; concatenates two lists into one
(check-expect (append-from-fold '(1 2 3) '(4 5)) '(1 2 3 4 5))
(check-expect (append-from-fold '() '(4 5)) '(4 5))
(check-expect (append-from-fold '(1 2 3) '()) '(1 2 3))

(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

; [List-of Number] -> Number
; compute the sum of numbers in l
(check-expect (sum '(1 2 3 4)) 10)

(define (sum l)
  (foldl + 0 l))

; [List-of Number] -> Number
; compute the product of numbers in l
(check-expect (product '(1 2 3 4)) 24)

(define (product l)
  (foldl * 1 l))

(define IMG1 (square 5 "solid" "red"))
(define IMG2 (square 10 "solid" "green"))
(define IMG3 (square 15 "solid" "blue"))

; [List-of Image] -> Image
; horizontally composes a list of Images
(check-expect (my-beside (list IMG1 IMG2 IMG3)) (beside IMG1 IMG2 IMG3))

(define (my-beside l)
  (foldr beside empty-image l))

; [List-of Image] -> Image
; stacks a list of images vertically
(check-expect (my-above (list IMG1 IMG2 IMG3)) (above IMG1 IMG2 IMG3))

(define (my-above l)
  (foldr above empty-image l))
