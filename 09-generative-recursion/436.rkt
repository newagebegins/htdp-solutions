;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |436|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define MAX 20)

; Posn -> Posn
; create a new food position that is not equal to p
; termination: if MAX > 1, the function always terminates because there is always some point in the square
; with corners [0,0] and (MAX,MAX) that is not equal to the given one and it will be generated sooner or later
; if MAX = 1, the only point is [0,0] and the function loops
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
