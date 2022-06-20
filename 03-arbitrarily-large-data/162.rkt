;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |162|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PAY 14) ; pay per hour

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (wage 28) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (wage 4) (cons (wage 2) '())))
(check-expect (wage* (cons 100 '())) (cons (wage 100) '()))
(check-error (wage* (cons 100.1 '())) "expected hours to be <= 100")

(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else
     (if (<= (first whrs) 100)
         (cons (wage (first whrs)) (wage* (rest whrs)))
         (error "expected hours to be <= 100"))]))

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* PAY h))
