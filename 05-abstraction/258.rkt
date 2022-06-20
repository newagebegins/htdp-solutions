;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |258|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; NELoP -> Posn
; extracts the last item from p
(check-expect (last (list (make-posn 0 0))) (make-posn 0 0))
(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))

(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else (last (rest p))]))

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect
 (render-poly MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))

(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly img p)
  (local (; Image NELoP -> Image
          ; connects the dots in p by rendering lines in img
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots img (rest p)) (first p) (second p))]))

          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red")))

    (render-line (connect-dots img p)
                 (first p)
                 (last p))))
