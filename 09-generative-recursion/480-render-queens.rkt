;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 480-render-queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn c r) denotes the square at
; the r-th row and c-th column

(define SIZE 30)
(define SQUARE (square SIZE "outline" "black"))
(define QUEEN (circle 10 "solid" "black"))

; N -> Image
; render a single row if an n by n chess board
(check-expect (render-row 1) SQUARE)
(check-expect (render-row 2) (beside SQUARE SQUARE))
(check-expect (render-row 3) (beside SQUARE SQUARE SQUARE))

(define (render-row n)
  (cond
    [(zero? n) empty-image]
    [else (beside SQUARE (render-row (sub1 n)))]))

; N -> Image
; render an n by n chess board
(check-expect (render-board 1) SQUARE)
(check-expect (render-board 2) (above (beside SQUARE SQUARE)
                                      (beside SQUARE SQUARE)))
(check-expect (render-board 3) (above (beside SQUARE SQUARE SQUARE)
                                      (beside SQUARE SQUARE SQUARE)
                                      (beside SQUARE SQUARE SQUARE)))

(define (render-board n)
  (local (; Image
          (define row (render-row n))
          ; N -> Image
          (define (render-rows i)
            (cond
              [(zero? i) empty-image]
              [else (above row (render-rows (sub1 i)))])))
    (render-rows n)))

; N [List-of QP] Image -> Image
; produce an image of an n by n chess board with the given image placed according to the given QPs
(check-expect (render-queens 2 (list (make-posn 0 0) (make-posn 1 1)) QUEEN)
              (overlay
               (place-image QUEEN (/ SIZE 2) (/ SIZE 2)
                            (place-image QUEEN (* SIZE 3/2) (* SIZE 3/2)
                                         (render-board 2)))
               (empty-scene (* SIZE 2) (* SIZE 2))))

(define (render-queens n qps img)
  (overlay
   (foldr (lambda (qp res)
            (place-image img
                         (* SIZE (+ 1/2 (posn-x qp)))
                         (* SIZE (+ 1/2 (posn-y qp)))
                         res))
          (render-board n)
          qps)
   (empty-scene (* SIZE n) (* SIZE n))))
