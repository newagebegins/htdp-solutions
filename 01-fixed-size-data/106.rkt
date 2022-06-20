;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |106|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define CAT (bitmap "../assets/cat.png"))
(define CAT2 (bitmap "../assets/cat2.png"))
(define CHAM (bitmap "../assets/cham.png"))
(define WIDTH 400)
(define HEIGHT 200)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))
(define DX 3) ; a number of pixels an animal moves per clock tick
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

(define-struct v-cat [x happiness])
; VCat is (make-v-cat XCoord Happiness)
; interp. a cat at distance x from the left edge of the scene, with given happiness level

(define-struct cham [x hap col])
; Cham is (make-cham XCoord Happiness Color)
; interp. a chameleon at distance x from the left edge of the scene, with given happiness level and color

; A VAnimal is either
; - a VCat
; - a Cham

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

; Happiness -> Happiness
; decrease happiness by -0.1 on tick
(check-expect (update-happiness 99.3) 99.2)

(define (update-happiness h)
  (change h -0.1))

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

; Cham -> Cham
; update the chameleon state each tick
(check-expect (update-cham (make-cham 0 100 "red")) (make-cham DX 99.9 "red"))
(check-expect (update-cham (make-cham 55 3 "green")) (make-cham (+ 55 DX) 2.9 "green"))
(check-expect (update-cham (make-cham 55 0 "blue")) (make-cham 55 0 "blue"))
(check-expect (update-cham (make-cham WIDTH 98.5 "red")) (make-cham DX 98.4 "red"))

(define (update-cham c)
  (make-cham (move-cham c) (update-happiness (cham-hap c)) (cham-col c)))

; VCat -> VCat
; update cat state each tick
(check-expect (update-cat (make-v-cat 0 100)) (make-v-cat DX 99.9))
(check-expect (update-cat (make-v-cat 55 3)) (make-v-cat (+ 55 DX) 2.9))
(check-expect (update-cat (make-v-cat 55 0)) (make-v-cat 55 0))
(check-expect (update-cat (make-v-cat WIDTH 98.5)) (make-v-cat DX 98.4))

(define (update-cat c)
  (make-v-cat (move-cat c) (update-happiness (v-cat-happiness c))))

; Happiness -> Image
; Renders a happiness gauge.
(define (render-gauge h)
  (overlay/align "left" "middle"
   (rectangle 100 20 "outline" "black")
   (rectangle h 20 "solid" "red")))

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

; Number -> Image
; draw the cat at the given x position
(define (draw-cat x)
  (place-image
   (cond
     [(odd? x) CAT]
     [else CAT2])
   x CTR-Y
   MTS))

; Cham -> Image
; render the chameleon and the happiness gauge
(check-expect (render-cham (make-cham 50 100 "red"))
              (overlay/align "middle" "top" (render-gauge 100) (draw-cham (make-cham 50 100 "red"))))
(check-expect (render-cham (make-cham 11 59 "green"))
              (overlay/align "middle" "top" (render-gauge 59) (draw-cham (make-cham 11 59 "green"))))

(define (render-cham c)
  (overlay/align "middle" "top" (render-gauge (cham-hap c)) (draw-cham c)))

; VCat -> Image
; render the cat and the happiness gauge
(check-expect (render-cat (make-v-cat 50 100)) (overlay/align "middle" "top" (render-gauge 100) (draw-cat 50)))
(check-expect (render-cat (make-v-cat 11 59)) (overlay/align "middle" "top" (render-gauge 59) (draw-cat 11)))

(define (render-cat c)
  (overlay/align "middle" "top" (render-gauge (v-cat-happiness c)) (draw-cat (v-cat-x c))))

; Cham -> Cham
; handle keypresses
(check-expect (handle-keys-cham (make-cham 1 10 "red") "down") (make-cham 1 12 "red"))
(check-expect (handle-keys-cham (make-cham 2 10 "green") "left") (make-cham 2 10 "green"))
(check-expect (handle-keys-cham (make-cham 2 10 "green") "r") (make-cham 2 10 "red"))
(check-expect (handle-keys-cham (make-cham 2 10 "blue") "g") (make-cham 2 10 "green"))
(check-expect (handle-keys-cham (make-cham 2 10 "red") "b") (make-cham 2 10 "blue"))

(define (handle-keys-cham c key)
  (cond
    [(string=? key "down") (make-cham (cham-x c) (change (cham-hap c) 2) (cham-col c))]
    [(string=? key "r") (make-cham (cham-x c) (cham-hap c) "red")]
    [(string=? key "g") (make-cham (cham-x c) (cham-hap c) "green")]
    [(string=? key "b") (make-cham (cham-x c) (cham-hap c) "blue")]
    [else c]))

; VCat -> VCat
; Changes happiness after keypresses.
; When down arrow is pressed happiness is decreased by 1/5.
; When up arrow is pressed happiness is increased by 1/3.
(check-expect (handle-keys-cat (make-v-cat 0 10) "down") (make-v-cat 0 (- 10 1/5)))
(check-expect (handle-keys-cat (make-v-cat 1 10) "up") (make-v-cat 1 (+ 10 1/3)))
(check-expect (handle-keys-cat (make-v-cat 2 10) "left") (make-v-cat 2 10))

(define (handle-keys-cat c key)
  (cond
    [(string=? key "down") (make-v-cat (v-cat-x c) (change (v-cat-happiness c) -1/5))]
    [(string=? key "up") (make-v-cat (v-cat-x c) (change (v-cat-happiness c) 1/3))]
    [else c]))

; VAnimal -> VAnimal
; update an animal each tick
(check-expect (animal-update (make-v-cat 55 3)) (make-v-cat (+ 55 DX) 2.9))
(check-expect (animal-update (make-cham WIDTH 98.5 "red")) (make-cham DX 98.4 "red"))

(define (animal-update a)
  (cond
    [(v-cat? a) (update-cat a)]
    [(cham? a) (update-cham a)]))

; VAnimal -> Image
; render an animal
(check-expect (animal-render (make-cham 50 100 "red")) (render-cham (make-cham 50 100 "red")))
(check-expect (animal-render (make-v-cat 50 100)) (render-cat (make-v-cat 50 100)))

(define (animal-render a)
  (cond
    [(v-cat? a) (render-cat a)]
    [(cham? a) (render-cham a)]))

; VAnimal KeyEvent -> VAnimal
; handle keys for an animal
(check-expect (animal-handle-keys (make-v-cat 0 10) "down") (handle-keys-cat (make-v-cat 0 10) "down"))
(check-expect (animal-handle-keys (make-cham 1 10 "red") "down") (handle-keys-cham (make-cham 1 10 "red") "down"))

(define (animal-handle-keys a ke)
  (cond
    [(v-cat? a) (handle-keys-cat a ke)]
    [(cham? a) (handle-keys-cham a ke)]))

; VAnimal -> VAnimal
; run the program with either:
; - (main (make-v-cat 0 100))
; - (main (make-cham 0 100 "green"))
(define (main a)
  (big-bang a
    [on-tick animal-update]
    [to-draw animal-render]
    [on-key animal-handle-keys]))
