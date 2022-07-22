;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 505-is-prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N [>=1] -> Boolean
; determines whether n is a prime number
(check-expect (is-prime? 1) #true)
(check-expect (is-prime? 2) #true)
(check-expect (is-prime? 3) #true)
(check-expect (is-prime? 4) #false)
(check-expect (is-prime? 5) #true)
(check-expect (is-prime? 6) #false)
(check-expect (is-prime? 71) #true)
(check-expect (is-prime? 97) #true)
(check-expect (is-prime? 70) #false)
(check-expect (is-prime? (* 29 3)) #false)

(define (is-prime? n0)
  (cond
    [(= n0 1) #true]
    [else
     (local (; N [>=1] -> Boolean
             (define (helper n)
               (cond
                 [(= n 1) #true]
                 [(zero? (remainder n0 n)) #false]
                 [else (helper (sub1 n))])))
       (helper (sub1 n0)))]))
