;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |145|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

; NEList-of-temperatures -> Boolean
; produce #true if the temperatures are sorted in descending order
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>? (cons -7.3 '())) #true)
(check-expect (sorted>? (cons 2 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3.3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 3.3 (cons 0 (cons 1 '())))) #false)
(check-expect (sorted>? (cons 0 (cons -3.2 (cons -5 '())))) #true)

(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [(cons? (rest l)) (and (> (first l) (first (rest l)))
                           (sorted>? (rest l)))]))
