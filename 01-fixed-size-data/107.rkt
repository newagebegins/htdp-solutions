;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |107|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct zoo [cat cham])
; A Zoo is a struct:
;   (make-zoo VCat Cham)
; interp. (make-zoo cat cham) represents two animals walking on the scene

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

; Cham Image -> Image
; draw the chameleon with a given color on top of the given image
(check-expect (render-cham (make-cham 55 100 "green") MTS)
              (overlay/align "right" "top" (render-gauge 100)
                             (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" "green"))
                                          55 CTR-Y MTS)))
(check-expect (render-cham (make-cham 22 50 "red") MTS)
              (overlay/align "right" "top" (render-gauge 50)
                             (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" "red"))
                                          22 CTR-Y MTS)))

(define (render-cham c im)
  (overlay/align "right" "top" (render-gauge (cham-hap c))
                 (place-image (overlay CHAM (rectangle (image-width CHAM) (image-height CHAM) "solid" (cham-col c)))
                              (cham-x c) CTR-Y im)))

; VCat Image -> Image
; render the cat on top of the given image
(check-expect (render-cat (make-v-cat 50 90) MTS)
              (overlay/align "left" "top" (render-gauge 90)
                             (place-image CAT2 50 CTR-Y MTS)))
(check-expect (render-cat (make-v-cat 51 87) MTS)
              (overlay/align "left" "top" (render-gauge 87)
                             (place-image CAT 51 CTR-Y MTS)))

(define (render-cat c i)
  (overlay/align "left" "top" (render-gauge (v-cat-happiness c))
                 (place-image
                  (cond
                    [(odd? (v-cat-x c)) CAT]
                    [else CAT2])
                  (v-cat-x c) CTR-Y
                  i)))

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

; Zoo -> Zoo
; update an animal each tick
(check-expect (update-zoo (make-zoo (make-v-cat 55 3) (make-cham WIDTH 98.5 "red")))
              (make-zoo (make-v-cat (+ 55 DX) 2.9) (make-cham DX 98.4 "red")))

(define (update-zoo z)
  (make-zoo (update-cat (zoo-cat z)) (update-cham (zoo-cham z))))

; Zoo -> Image
; render an animal
(check-expect (render-zoo (make-zoo (make-v-cat 0 100) (make-cham 100 80 "red")))
              (render-cat (make-v-cat 0 100)
                          (render-cham (make-cham 100 80 "red") MTS)))
(define (render-zoo a)
  (render-cat (zoo-cat a)
              (render-cham (zoo-cham a) MTS)))

; Zoo KeyEvent -> Zoo
; handle keys for the animals
(check-expect (handle-keys-zoo (make-zoo (make-v-cat 0 10) (make-cham 1 10 "red")) "down")
              (make-zoo (handle-keys-cat (make-v-cat 0 10) "down")
                        (handle-keys-cham (make-cham 1 10 "red") "down")))

(define (handle-keys-zoo z ke)
  (make-zoo (handle-keys-cat (zoo-cat z) ke)
            (handle-keys-cham (zoo-cham z) ke)))

; Zoo -> Zoo
; run the program with (main (make-zoo (make-v-cat 0 100) (make-cham 70 50 "green")))
(define (main z)
  (big-bang z
    [on-tick update-zoo]
    [to-draw render-zoo]
    [on-key handle-keys-zoo]))
