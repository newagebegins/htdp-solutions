;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 356-bsl-fun-expr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name expr])

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)
; - (make-fun Symbol BSL-fun-expr)

; (k (+ 1 1))
(define E1 (make-fun 'k (make-add 1 1)))

; (* 5 (k (+ 1 1)))
(define E2 (make-mul 5 (make-fun 'k (make-add 1 1))))

; (* (i 5) (k (+ 1 1)))
(define E3 (make-mul (make-fun 'i 5) (make-fun 'k (make-add 1 1))))
