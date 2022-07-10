;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |430|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort '(11 8 14 7) <) '(7 8 11 14))
(check-expect (quick-sort '(11 8 14 7) >) '(14 11 8 7))

(define (quick-sort alon cmp)
  (cond
    [(<= (length alon) 1) alon]
    [else (local ((define pivot (first alon))
                  (define smallers (filter (lambda (x) (cmp x pivot)) alon))
                  (define largers (filter (lambda (x) (and (not (cmp x pivot))
                                                           (not (= x pivot)))) alon)))
            (append (quick-sort smallers cmp)
                    (list pivot)
                    (quick-sort largers cmp)))]))
