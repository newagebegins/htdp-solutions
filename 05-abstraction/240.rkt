;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |240|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

(define LSTR1 "hello")
(define LSTR2 (make-layer LSTR1))
(define LSTR3 (make-layer LSTR2))
    
; An LNum is one of: 
; – Number
; – (make-layer LNum)

(define LNUM1 7)
(define LNUM2 (make-layer LNUM1))
(define LNUM3 (make-layer LNUM2))

; An [L X] is one of:
; - X
; - (make-layer L)

; [L String]
; [L Number]
