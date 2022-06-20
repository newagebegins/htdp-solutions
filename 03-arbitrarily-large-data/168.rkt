;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |168|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; - (cons Posn '())
; - (cons Posn List-of-posns)
; interp. a list of positions

; List-of-posns -> List-of-posns
; translate positions down by 1 pixel
(check-expect (translate '()) '())
(check-expect (translate (cons (make-posn 1 2) '())) (cons (make-posn 1 3) '()))
(check-expect (translate (cons (make-posn 9 8.5) (cons (make-posn 1 2) '())))
              (cons (make-posn 9 9.5) (cons (make-posn 1 3) '())))

(define (translate l)
  (cond
    [(empty? l) '()]
    [else (cons (make-posn (posn-x (first l)) (add1 (posn-y (first l)))) (translate (rest l)))]))
