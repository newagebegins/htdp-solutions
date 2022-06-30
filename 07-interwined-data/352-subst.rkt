;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 352-subst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr Symbol Number -> BSL-var-expr
; replace all occurrences of x by v in ex
(check-expect (subst 8 'a 7) 8)
(check-expect (subst 'a 'a 7) 7)
(check-expect (subst 'b 'a 7) 'b)
(check-expect (subst (make-add (make-mul 'x 'x) 'y) 'x 3)
              (make-add (make-mul 3 3) 'y))
(check-expect (subst (make-mul (make-add 'y 7) (make-add 1 'y)) 'y 99)
              (make-mul (make-add 99 7) (make-add 1 99)))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v) (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v) (subst (mul-right ex) x v))]))
