;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |160|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; Constraint: If s is a Son.R, no number occurs twice in s

; Number Son.L -> Son.L
; add a number x to the set s
(check-expect (set+.L 1 '()) (cons 1 '()))
(check-expect (set+.L 1 (cons 2 '())) (cons 1 (cons 2 '())))
(check-expect (set+.L 1 (cons 1 '())) (cons 1 (cons 1 '())))

(define (set+.L x s)
  (cons x s))
  
; Number Son.R -> Son.R
; add a number x to the set s
(check-expect (set+.R 1 '()) (cons 1 '()))
(check-expect (set+.R 1 (cons 2 '())) (cons 1 (cons 2 '())))
(check-expect (set+.R 1 (cons 1 '())) (cons 1 '()))

(define (set+.R x s)
  (if (member? x s)
      s
      (cons x s)))
