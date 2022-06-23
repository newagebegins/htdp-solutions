;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |285|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define USD-PER-EUR 1.06)

; [List-of Number] -> [List-of Number]
; convert a list of US$ amounts into a list of euro amounts
(check-expect (convert-euro '(1.0 2.0 0.5)) (list (/ 1.0 USD-PER-EUR)
                                                  (/ 2.0 USD-PER-EUR)
                                                  (/ 0.5 USD-PER-EUR)))

(define (convert-euro usd)
  (map (lambda (x) (/ x USD-PER-EUR)) usd))

; [List-of Number] -> [List-of Number]
; convert a list of Fahrenheit measurements to a list of Celsius measurements
(check-expect (convertFC '(32 41)) '(0 5))

(define (convertFC f)
  (map (lambda (x) (* (- x 32) 5/9)) f))

; [List-of Posn] -> [List-of [List-of Number]]
; translate a list of Posns into a list of lists of pairs of numbers
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              '((1 2) (3 4)))

(define (translate l)
  (map (lambda (p) (list (posn-x p) (posn-y p))) l))