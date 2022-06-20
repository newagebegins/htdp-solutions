;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |239|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A [List X Y] is a list:
;   (cons X (cons Y '())

; [List Number Number]
(cons 1 (cons 2 '()))

; [List Number 1String]
(cons 1 (cons "a" '()))

; [List String Boolean]
(cons "a" (cons #true '()))
