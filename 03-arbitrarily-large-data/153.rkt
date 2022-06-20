;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |153|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; An N is one of:
; - 0
; - (add1 N)
; interp. represents the counting numbers

(define SI1 (circle 5 "solid" "green"))  ; sample image 1
(define SI2 (square 6 "outline" "blue")) ; sample image 2

; N Image -> Image
; produce a column — a vertical arrangement — of n copies of img.
(check-expect (col 0 SI1) empty-image)
(check-expect (col 1 SI1) SI1)
(check-expect (col 2 SI1) (above SI1 SI1))
(check-expect (col 3 SI2) (above SI2 SI2 SI2))

(define (col n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (above img (col (sub1 n) img))]))

; N Image -> Image
; produce a row — a horizontal arrangement — of n copies of img.
(check-expect (row 0 SI1) empty-image)
(check-expect (row 1 SI1) SI1)
(check-expect (row 2 SI1) (beside SI1 SI1))
(check-expect (row 3 SI2) (beside SI2 SI2 SI2))

(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (beside img (row (sub1 n) img))]))

(define COLUMNS 10)
(define ROWS 20)
(define SQSZ 10) ; square size
(define BG (overlay (row COLUMNS (col ROWS (square SQSZ "outline" "black")))
                    (empty-scene (* COLUMNS SQSZ) (* ROWS SQSZ))))
(define DOT (circle 3 "solid" "red"))

; A List-of-posns is one of:
; - '()
; - (cons Posn List-of-posns)
; interp. a list of positions

; List-of-posns -> Image
; produce an image of a lecture hall with red dots added as specified by the posns in the list
(check-expect (add-balloons '()) BG)
(check-expect (add-balloons (cons (make-posn 10 20) '())) (place-image DOT 10 20 BG))
(check-expect (add-balloons (cons (make-posn 65 28) '())) (place-image DOT 65 28 BG))
(check-expect (add-balloons (cons (make-posn 30 88) (cons (make-posn 65 28) '())))
                            (place-image DOT 30 88
                                         (place-image DOT 65 28 BG)))

(define (add-balloons l)
  (cond
    [(empty? l) BG]
    [(cons? l) (place-image DOT
                            (posn-x (first l))
                            (posn-y (first l))
                            (add-balloons (rest l)))]))
