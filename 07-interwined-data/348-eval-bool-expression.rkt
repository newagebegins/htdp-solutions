;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 348-eval-bool-expression) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct bsl-and [x1 x2])
(define-struct bsl-or [x1 x2])
(define-struct bsl-not [x])

; BSL-bool-expr is one of:
; - #true
; - #false
; - (make-bsl-and BSL-bool-expr BSL-bool-expr)
; - (make-bsl-or BSL-bool-expr BSL-bool-expr)
; - (make-bsl-not BSL-bool-expr)

; BSL-bool-expr -> Boolean
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression (make-bsl-and #true #true)) #true)
(check-expect (eval-bool-expression (make-bsl-and #true #false)) #false)
(check-expect (eval-bool-expression (make-bsl-or #true #false)) #true)
(check-expect (eval-bool-expression (make-bsl-or #false #false)) #false)
(check-expect (eval-bool-expression (make-bsl-not #true)) #false)
(check-expect (eval-bool-expression (make-bsl-not #false)) #true)
(check-expect (eval-bool-expression (make-bsl-or (make-bsl-and #true #true) (make-bsl-not #true))) #true)
(check-expect (eval-bool-expression (make-bsl-and (make-bsl-and #true #true) (make-bsl-not #true))) #false)
(check-expect (eval-bool-expression (make-bsl-not (make-bsl-and (make-bsl-or #true #false) #true))) #false)

(define (eval-bool-expression expr)
  (cond
    [(boolean? expr) expr]
    [(bsl-and? expr) (and (eval-bool-expression (bsl-and-x1 expr)) (eval-bool-expression (bsl-and-x2 expr)))]
    [(bsl-or? expr) (or (eval-bool-expression (bsl-or-x1 expr)) (eval-bool-expression (bsl-or-x2 expr)))]
    [(bsl-not? expr) (not (eval-bool-expression (bsl-not-x expr)))]))
