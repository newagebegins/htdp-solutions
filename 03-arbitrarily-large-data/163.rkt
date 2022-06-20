;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |163|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; Number -> Number
; convert Fahrenheit to Celsius
(define (FC f)
  (* (- f 32) 5/9))
         
; List-of-numbers -> List-of-numbers
; converts a list of measurements in Fahrenheit to a list of Celsius measurements
(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 30 '())) (cons (FC 30) '()))
(check-expect (convertFC (cons 0 (cons 30 '()))) (cons (FC 0) (cons (FC 30) '())))

(define (convertFC l)
  (cond
    [(empty? l) '()]
    [else (cons (FC (first l)) (convertFC (rest l)))]))
