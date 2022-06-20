;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |167|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; - (cons Posn '())
; - (cons Posn List-of-posns)
; interp. a list of positions

; List-of-posns -> Number
; compute the sum of all of x-coordinates of posns in the list
(check-expect (sum (cons (make-posn 1 2) '())) 1)
(check-expect (sum (cons (make-posn 3 2) '())) 3)
(check-expect (sum (cons (make-posn 4 5) (cons (make-posn 3 2) '()))) 7)

(define (sum l)
  (cond
    [(empty? (rest l)) (posn-x (first l))]
    [else (+ (posn-x (first l)) (sum (rest l)))]))
