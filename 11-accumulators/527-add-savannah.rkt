;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 527-add-savannah) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define MIN-LEN 7)
(define COLOR "red")
(define LEFT-K 0.33)
(define RIGHT-K 0.66)

; Image N N N Number -> Image
; adds a fractal Savannah tree to the given image
; x, y - coordinates of a line's base point
; len - the length of the line
; ang - the angle of the line (in radians)
(define (add-savannah scene0 x y len ang)
  (cond
    [(< len MIN-LEN) scene0]
    [else
     (local
       ((define end-x (+ x (* len (cos ang))))
        (define end-y (- y (* len (sin ang))))
        (define left-x  (+ x (* LEFT-K (- end-x x))))
        (define left-y  (+ y (* LEFT-K (- end-y y))))
        (define right-x (+ x (* RIGHT-K (- end-x x))))
        (define right-y (+ y (* RIGHT-K (- end-y y))))
        (define scene1 (scene+line scene0 x y end-x end-y COLOR))
        (define scene2 (add-savannah scene1 left-x left-y (* 2/3 len) (+ ang 0.18)))
        (define scene3 (add-savannah scene2 right-x right-y (* 0.8 len) (- ang 0.15))))
       scene3)]))

(add-savannah (empty-scene 200 200) 100 200 60 (/ pi 2))
