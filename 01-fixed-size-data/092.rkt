;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |092|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define CHAM (bitmap "../assets/cham.png"))

(define WIDTH 400)
(define HEIGHT 200)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define DX 3) ; a number of pixels the chameleon moves per clock tick
(define CTR-Y (/ HEIGHT 2))

; XCoord is Natural[0, WIDTH]
; interp. x coordinate of the animal

; Happiness is a Natural[0, 100]
; interp. happiness level of the animal

; Color is one of:
; - "red"
; - "green"
; - "blue"
; interp. color of the chameleon

(define-struct cham [x hap col])
; Cham is (make-cham XCoord Happiness Color)
; interp. a chameleon at distance x from the left edge of the scene, with given happiness level and color

; Cham -> Cham
; run the program with (main (make-cham 0 100 "green"))
(define (main c)
  (big-bang c
    [on-tick update]
    [to-draw render]
    [on-key handle-keys]))

; Cham -> Cham
; update the chameleon state each tick
(check-expect (update (make-cham 0 100 "red")) (make-cham DX 99.9 "red"))
(check-expect (update (make-cham 55 3 "green")) (make-cham (+ 55 DX) 2.9 "green"))
(check-expect (update (make-cham 55 0 "blue")) (make-cham 55 0 "blue"))
(check-expect (update (make-cham WIDTH 98.5 "red")) (make-cham DX 98.4 "red"))

(define (update c)
  (make-cham (move-cham c) (update-happiness (cham-hap c)) (cham-col c)))

; Cham -> Image
; render the chameleon and the happiness gauge
(check-expect (render (make-cham 50 100 "red"))
              (overlay/align "middle" "top" (render-gauge 100) (draw-cham (make-cham 50 100 "red"))))
(check-expect (render (make-cham 11 59 "green"))
              (overlay/align "middle" "top" (render-gauge 59) (draw-cham (make-cham 11 59 "green"))))

(define (render c)
  (overlay/align "middle" "top" (render-gauge (cham-hap c)) (draw-cham c)))

; Cham -> Number
; Given a chameleon produce new x coordinate a few pixels to the right.
; Whenever the chameleon disappears on the right, it reappears on the left.
; Chameleon stops moving when happiness is 0.
(check-expect (move-cham (make-cham 0 100 "red")) DX)
(check-expect (move-cham (make-cham 10 100 "green")) (+ 10 DX))
(check-expect (move-cham (make-cham WIDTH 100 "blue")) DX)
(check-expect (move-cham (make-cham 10 0 "red")) 10)

(define (move-cham c)
  (if (> (cham-hap c) 0)
      (modulo (+ (cham-x c) DX) WIDTH)
      (cham-x c)))

; Cham -> Image
; draw the chameleon at the given x position with a given color
(check-expect (draw-cham (make-cham 55 100 "green"))
              (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" "green"))
                           55 CTR-Y MTS))
(check-expect (draw-cham (make-cham 22 100 "red"))
              (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" "red"))
                           22 CTR-Y MTS))

(define (draw-cham c)
  (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" (cham-col c)))
               (cham-x c) CTR-Y MTS))

; Happiness -> Happiness
; decrease happiness by -0.1 on tick
(check-expect (update-happiness 99.3) 99.2)

(define (update-happiness h)
  (change h -0.1))

; Cham -> Cham
; handle keypresses
(check-expect (handle-keys (make-cham 1 10 "red") "down") (make-cham 1 12 "red"))
(check-expect (handle-keys (make-cham 2 10 "green") "left") (make-cham 2 10 "green"))
(check-expect (handle-keys (make-cham 2 10 "green") "r") (make-cham 2 10 "red"))
(check-expect (handle-keys (make-cham 2 10 "blue") "g") (make-cham 2 10 "green"))
(check-expect (handle-keys (make-cham 2 10 "red") "b") (make-cham 2 10 "blue"))

(define (handle-keys c key)
  (cond
    [(string=? key "down") (make-cham (cham-x c) (change (cham-hap c) 2) (cham-col c))]
    [(string=? key "r") (make-cham (cham-x c) (cham-hap c) "red")]
    [(string=? key "g") (make-cham (cham-x c) (cham-hap c) "green")]
    [(string=? key "b") (make-cham (cham-x c) (cham-hap c) "blue")]
    [else c]))

; Happiness -> Image
; Renders a happiness gauge.
(check-expect (render-gauge 50)
              (overlay/align "left" "middle"
                             (rectangle 100 20 "outline" "black")
                             (rectangle 50 20 "solid" "red")))
(define (render-gauge h)
  (overlay/align "left" "middle"
                 (rectangle 100 20 "outline" "black")
                 (rectangle h 20 "solid" "red")))

; Happiness -> Happiness
; Change happiness h by an amount d.
; The result is always in the range [0,100].
(check-expect (change 99.3 -0.1) 99.2)
(check-expect (change 99.9 0.1) 100)
(check-expect (change 0 10) 10)
(check-expect (change 0 -0.1) 0)
(check-expect (change 100 0.1) 100)
(define (change h d)
  (cond
    [(< (+ h d) 0) 0]
    [(> (+ h d) 100) 100]
    [else (+ h d)]))
