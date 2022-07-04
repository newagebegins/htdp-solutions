;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |371|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr is a list:
; – (cons Symbol Xexpr-body)
; – (cons Symbol (cons [List-of Attribute] Xexpr-body))

; A Xexpr-body is a list of Xexprs and Strings:
; - '()
; - (cons Xexpr Xexpr-body)
; - (cons String Xexpr-body)

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define UL1 '(ul
              (li ((color "red"))
                  "hello"
                  (br)
                  "123")
              (li "world")))
