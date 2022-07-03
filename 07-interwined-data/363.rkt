;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |363|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr is (cons Symbol Attributes-and-body)

; Attributes-and-body is one of:
; - Body
; - (cons Attributes Body)

; Body is one of:
; - '()
; - (cons Xexpr Body)

; Attributes is one of:
; - '()
; - (cons Attribute Attributes)

; Attribute is a list of two items:
;   (cons Symbol (cons String '()))
