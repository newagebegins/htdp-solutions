;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 459-integrate-rectangles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.01)
(define R 1000)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds

(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)

(define (integrate-rectangles f a b)
  (local ((define W (/ (- b a) R))
          (define S (/ W 2))
          ; N -> Number
          (define (sum-areas i)
            (cond
              [(zero? i) 0]
              [else (+ (* W (f (+ a (* (sub1 i) W) S)))
                       (sum-areas (sub1 i)))])))
    (sum-areas R)))
