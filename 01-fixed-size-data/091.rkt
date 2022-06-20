;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |091|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Happiness is a Number[0, 100]
; interp. happiness value for the happiness gauge

; Direction is on of:
; -  1 (moving right)
; - -1 (moving left)
; interp. direction of movement

(define-struct v-cat [x dir happiness])
; VCat is (make-v-cat Number Number Happiness)
; interp. x - distance from the left edge of the scene
;         dir - direction of movement
;         happiness - happiness level

(define CAT (bitmap "../assets/cat.png"))
(define CAT2 (bitmap "../assets/cat2.png"))

(define WIDTH 400)
(define HEIGHT 200)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define DX 3) ; a number of pixels the cat moves per clock tick
(define CAT-Y (/ HEIGHT 2))

; VCat -> VCat
; run the program with (happy-cat (make-v-cat 0 1 100))
(define (happy-cat c)
  (big-bang c
    [on-tick update]
    [to-draw render]
    [on-key handle-keys]))

; VCat -> Number
; produce new x based on current x, DX and direction
(check-expect (new-x (make-v-cat 12 1 100)) (+ 12 DX))
(check-expect (new-x (make-v-cat 11 -1 55)) (- 11 DX))

(define (new-x c)
  (+ (v-cat-x c) (* (v-cat-dir c) DX)))

; VCat -> VCat
; Produce new cat with updated position.
; When cat reaches the edge of the scene, change the direction.
; Cat stops moving when happiness is 0.
(check-expect (move-cat (make-v-cat 0 1 100)) (make-v-cat DX 1 100))
(check-expect (move-cat (make-v-cat 10 1 100)) (make-v-cat (+ 10 DX) 1 100))
(check-expect (move-cat (make-v-cat (- WIDTH 1) 1 100)) (make-v-cat WIDTH -1 100))

(check-expect (move-cat (make-v-cat 1 -1 100)) (make-v-cat 0 1 100))
(check-expect (move-cat (make-v-cat 10 -1 100)) (make-v-cat (- 10 DX) -1 100))

(check-expect (move-cat (make-v-cat 15 1 0)) (make-v-cat 15 1 0))

(define (move-cat c)
  (if (> (v-cat-happiness c) 0)
      (cond
        [(> (new-x c) WIDTH) (make-v-cat WIDTH -1 (v-cat-happiness c))]
        [(< (new-x c) 0) (make-v-cat 0 1 (v-cat-happiness c))]
        [else (make-v-cat (new-x c) (v-cat-dir c) (v-cat-happiness c))])
      c))

; VCat -> VCat
; update cat state each tick
(check-expect (update (make-v-cat 0 1 100)) (make-v-cat DX 1 99.9))
(check-expect (update (make-v-cat 55 1 3)) (make-v-cat (+ 55 DX) 1 2.9))
(check-expect (update (make-v-cat 55 1 0)) (make-v-cat 55 1 0))
(check-expect (update (make-v-cat 58 -1 0)) (make-v-cat 58 -1 0))
(check-expect (update (make-v-cat (- WIDTH 1) 1 98.5)) (make-v-cat WIDTH -1 98.4))
(check-expect (update (make-v-cat 1 -1 98.5)) (make-v-cat 0 1 98.4))

(define (update c)
  (move-cat (update-happiness c)))

; VCat -> Image
; render the cat and the happiness gauge
(check-expect (render (make-v-cat 50 1 100)) (overlay/align "middle" "top" (render-gauge 100) (draw-cat 50)))
(check-expect (render (make-v-cat 11 1 59)) (overlay/align "middle" "top" (render-gauge 59) (draw-cat 11)))

(define (render c)
  (overlay/align "middle" "top" (render-gauge (v-cat-happiness c)) (draw-cat (v-cat-x c))))

; Number -> Image
; draw the cat at the given x position
(define (draw-cat x)
  (place-image
   (cond
     [(odd? x) CAT]
     [else CAT2])
   x CAT-Y
   MTS))

; VCat -> VCat
; decrease happiness by -0.1 on tick
(check-expect (update-happiness (make-v-cat 12 1 99.3)) (make-v-cat 12 1 99.2))

(define (update-happiness c)
  (make-v-cat (v-cat-x c) (v-cat-dir c) (change (v-cat-happiness c) -0.1)))

; VCat -> VCat
; Changes happiness after keypresses.
; When down arrow is pressed happiness is decreased by 1/5.
; When up arrow is pressed happiness is increased by 1/3.
(check-expect (handle-keys (make-v-cat 0 1 10) "down") (make-v-cat 0 1 (- 10 1/5)))
(check-expect (handle-keys (make-v-cat 1 -1 10) "up") (make-v-cat 1 -1 (+ 10 1/3)))
(check-expect (handle-keys (make-v-cat 2 1 10) "left") (make-v-cat 2 1 10))

(define (handle-keys c key)
  (cond
    [(string=? key "down") (make-v-cat (v-cat-x c) (v-cat-dir c) (change (v-cat-happiness c) -1/5))]
    [(string=? key "up") (make-v-cat (v-cat-x c) (v-cat-dir c) (change (v-cat-happiness c) 1/3))]
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
