;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |432|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define MAX 20)

; Posn -> Posn
; create a new food position that is not equal to p
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)

(define (food-create p)
  (local (; Posn -> Posn
          ; generative recursion
          (define (food-check-create candidate)
            (if (equal? p candidate) (food-create p) candidate)))
    (food-check-create (make-posn (random MAX) (random MAX)))))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
