;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |305|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define EUR-PER-USD (/ 1 1.06))

; [List-of Number] -> [List-of Number]
; convert a list of USD amounts into a list of EUR amounts
(check-expect (convert-euro '(0 1.06 2.12)) '(0 1 2))

(define (convert-euro usd-amounts)
  (for/list ([usd usd-amounts])
    (* usd EUR-PER-USD)))
