;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |256|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(define (argmax f lx) ...)

(argmax length '("hello" "test" "abracadabra" "foo")) == "abracadabra"

; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-i, 
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
(define (argmin f lx) ...)

(argmin length '("hello" "test" "abracadabra" "foo")) == "foo"
