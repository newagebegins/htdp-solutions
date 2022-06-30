;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 353-numeric) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr -> Boolean
; return #true if ex is a BSL-expr (doesn't contain symbols)
(check-expect (numeric? 1) #true)
(check-expect (numeric? 'a) #false)
(check-expect (numeric? (make-add 1 2)) #true)
(check-expect (numeric? (make-add 1 'x)) #false)
(check-expect (numeric? (make-mul 1 2)) #true)
(check-expect (numeric? (make-mul 1 'x)) #false)
(check-expect (numeric? (make-add (make-mul 'x 3) 5)) #false)
(check-expect (numeric? (make-add (make-mul 6 3) 5)) #true)
(check-expect (numeric? (make-mul (make-add 'x 3) 5)) #false)
(check-expect (numeric? (make-mul (make-add 7 3)
                                  (make-mul 3 (make-add 'a 'b)))) #false)

(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(symbol? ex) #false]
    [(add? ex) (and (numeric? (add-left ex)) (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex)) (numeric? (mul-right ex)))]))
