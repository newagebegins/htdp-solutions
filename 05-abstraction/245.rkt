;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |245|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (function=at-1.2-3-and-5.775? max min) #true)
(check-expect (function=at-1.2-3-and-5.775? sqrt sqr) #false)

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.775) (f2 -5.775))))
