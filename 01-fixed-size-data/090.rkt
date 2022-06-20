;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |090|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Happiness is a Number[0, 100]

(define-struct v-cat [x happiness])
; VCat is (make-v-cat Number Happiness)
; interp. a cat at distance x from the left edge of the scene, with given happiness level

(define CAT (bitmap "../assets/cat.png"))
(define CAT2 (bitmap "../assets/cat2.png"))

(define WIDTH 400)
(define HEIGHT 200)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define DX 3) ; a number of pixels the cat moves per clock tick
(define CAT-Y (/ HEIGHT 2))

; VCat -> VCat
; run the program with (happy-cat (make-v-cat 0 100))
(define (happy-cat c)
  (big-bang c
    [on-tick update]
    [to-draw render]
    [on-key handle-keys]))

; VCat -> VCat
; update cat state each tick
(check-expect (update (make-v-cat 0 100)) (make-v-cat DX 99.9))
(check-expect (update (make-v-cat 55 3)) (make-v-cat (+ 55 DX) 2.9))
(check-expect (update (make-v-cat 55 0)) (make-v-cat 55 0))
(check-expect (update (make-v-cat WIDTH 98.5)) (make-v-cat DX 98.4))

(define (update c)
  (make-v-cat (move-cat c) (update-happiness (v-cat-happiness c))))

; VCat -> Image
; render the cat and the happiness gauge
(check-expect (render (make-v-cat 50 100)) (overlay/align "middle" "top" (render-gauge 100) (draw-cat 50)))
(check-expect (render (make-v-cat 11 59)) (overlay/align "middle" "top" (render-gauge 59) (draw-cat 11)))

(define (render c)
  (overlay/align "middle" "top" (render-gauge (v-cat-happiness c)) (draw-cat (v-cat-x c))))

; VCat -> Number
; Given a cat produce new x coordinate a few pixels to the right.
; Whenever the cat disappears on the right, it reappears on the left.
; Cat stops moving when happiness is 0.
(check-expect (move-cat (make-v-cat 0 100)) DX)
(check-expect (move-cat (make-v-cat 10 100)) (+ 10 DX))
(check-expect (move-cat (make-v-cat WIDTH 100)) DX)
(check-expect (move-cat (make-v-cat 10 0)) 10)

(define (move-cat c)
  (if (> (v-cat-happiness c) 0)
      (modulo (+ (v-cat-x c) DX) WIDTH)
      (v-cat-x c)))

; Number -> Image
; draw the cat at the given x position
(define (draw-cat x)
  (place-image
   (cond
     [(odd? x) CAT]
     [else CAT2])
   x CAT-Y
   MTS))

; Happiness -> Happiness
; decrease happiness by -0.1 on tick
(check-expect (update-happiness 99.3) 99.2)

(define (update-happiness h)
  (change h -0.1))

; VCat -> VCat
; Changes happiness after keypresses.
; When down arrow is pressed happiness is decreased by 1/5.
; When up arrow is pressed happiness is increased by 1/3.
(check-expect (handle-keys (make-v-cat 0 10) "down") (make-v-cat 0 (- 10 1/5)))
(check-expect (handle-keys (make-v-cat 1 10) "up") (make-v-cat 1 (+ 10 1/3)))
(check-expect (handle-keys (make-v-cat 2 10) "left") (make-v-cat 2 10))

(define (handle-keys c key)
  (cond
    [(string=? key "down") (make-v-cat (v-cat-x c) (change (v-cat-happiness c) -1/5))]
    [(string=? key "up") (make-v-cat (v-cat-x c) (change (v-cat-happiness c) 1/3))]
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
