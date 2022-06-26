;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 316-atom) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of:
; - Number
; - String
; - Symbol

; Any -> Boolean
; return #true if x is an Atom
(check-expect (atom? 7) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'hello) #true)
(check-expect (atom? '()) #false)

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))
