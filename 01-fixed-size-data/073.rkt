;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |073|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Posn Number -> Posn
; produces a Posn like p with n in the x field
(check-expect (posn-up-x (make-posn 1 2) 3) (make-posn 3 2))
(define (posn-up-x p n)
  (make-posn n (posn-y p)))
