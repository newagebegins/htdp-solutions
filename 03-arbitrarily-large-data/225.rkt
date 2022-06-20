;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |225|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 200)

(define CENTER-X (* WIDTH 0.5))
(define CENTER-Y (* HEIGHT 0.5))

(define PLANE-Y (* HEIGHT 0.2))
(define PLANE-SPEED 3)
(define PLANE-WIDTH 20)
(define PLANE-MIN-X (/ PLANE-WIDTH 2))
(define PLANE-MAX-X (- WIDTH (/ PLANE-WIDTH 2)))

(define DROP-SPEED 4)
(define DROP-RADIUS 6)
(define DROP-MIN-Y (+ PLANE-Y DROP-RADIUS))
(define DROP-MAX-Y (- HEIGHT DROP-RADIUS))

(define FIRE-Y (- HEIGHT 8))
(define TICKS-PER-FIRE 40)

(define COL-RAD 8) ; collision radius

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define PLANE (rectangle PLANE-WIDTH 10 "solid" "gray"))
(define DROP (circle DROP-RADIUS "solid" "blue"))
(define FIRE (triangle 15 "solid" "red"))

; A Direction is one of:
; - "left"
; - "right"

; A Fire is a Number
; x coordinate of the fire

(define FIRE1 CENTER-X)
(define FIRE2 (+ CENTER-X (* COL-RAD 5)))

; A List-of-fires is one of:
; - '()
; - (cons Fire List-of-fires)

(define-struct plane [x dir])
; A Plane is a structure:
;   (make-plane Number Direction)
; interp. represents x coordinate of the plane and its direction of movement

(define PLANE1 (make-plane CENTER-X "right"))

(define-struct drop [x y])
; A Drop is a structure:
;   (make-drop Number Number)
; interp. represents x and y coordinates of the water drop

; A List-of-drops is one of:
; - '()
; - (cons Drop List-of-drops)

; An N is one of:
; - 0
; - (add1 N)
; interp. counting numbers

(define-struct game [plane drops fires ticks])
; A Game is a structure:
;   (make-game Plane List-of-drops List-of-fires N)

; Plane Image -> Image
(check-expect (render-plane (make-plane CENTER-X "right") BACKGROUND)
              (place-image PLANE CENTER-X PLANE-Y BACKGROUND))

(define (render-plane p img)
  (place-image PLANE (plane-x p) PLANE-Y img))

; Drop Image -> Image
; render a single water drop
(check-expect (render-drop (make-drop CENTER-X CENTER-Y) BACKGROUND)
              (place-image DROP CENTER-X CENTER-Y BACKGROUND))

(define (render-drop w img)
  (place-image DROP (drop-x w) (drop-y w) img))

; List-of-drops Image -> Image
(check-expect (render-drops '() BACKGROUND) BACKGROUND)
(check-expect (render-drops (list (make-drop CENTER-X CENTER-Y)
                                  (make-drop (* WIDTH 0.6) (* HEIGHT 0.7)))
                            BACKGROUND)
              (render-drop (make-drop CENTER-X CENTER-Y)
                           (render-drop (make-drop (* WIDTH 0.6) (* HEIGHT 0.7))
                                        BACKGROUND)))

(define (render-drops l img)
  (cond
    [(empty? l) img]
    [else (render-drop (first l) (render-drops (rest l) img))]))

; Fire Image -> Image
(check-expect (render-fire CENTER-X BACKGROUND)
              (place-image FIRE CENTER-X FIRE-Y BACKGROUND))

(define (render-fire f img)
  (place-image FIRE f FIRE-Y img))

; List-of-fires Image -> Image
(check-expect (render-fires '() BACKGROUND) BACKGROUND)
(check-expect (render-fires (list CENTER-X (* WIDTH 0.7)) BACKGROUND)
              (render-fire CENTER-X
                           (render-fire (* WIDTH 0.7)
                                        BACKGROUND)))

(define (render-fires l img)
  (cond
    [(empty? l) img]
    [else (render-fire (first l) (render-fires (rest l) img))]))

; Game -> Image
(check-expect (render-game
               (make-game (make-plane CENTER-X "right")
                          (list (make-drop CENTER-X CENTER-Y)
                                (make-drop (* WIDTH 0.7) (* HEIGHT 0.7)))
                          (list (* WIDTH 0.3) (* WIDTH 0.8))
                          0))
              (render-plane (make-plane CENTER-X "right")
                            (render-drops (list (make-drop CENTER-X CENTER-Y)
                                                (make-drop (* WIDTH 0.7) (* HEIGHT 0.7)))
                                          (render-fires (list (* WIDTH 0.3) (* WIDTH 0.8))
                                                        BACKGROUND))))

(define (render-game g)
  (render-plane (game-plane g)
                (render-drops (game-drops g)
                              (render-fires (game-fires g)
                                            BACKGROUND))))

; Game -> Boolean
; return #true when all fires have been extinguished
(check-expect (game-over? (make-game PLANE1 '() '() 0)) #true)
(check-expect (game-over? (make-game PLANE1 '() (list FIRE1) 0)) #false)

(define (game-over? g)
  (empty? (game-fires g)))

; Drop -> Boolean
(define (drop-collided-with-ground? d)
  (>= (drop-y d) DROP-MAX-Y))

; Drop Fire -> Number
; calculate the distance between the water drop and the fire
(check-expect (distance (make-drop 0 0) 0) FIRE-Y)

(define (distance d f)
  (sqrt (+ (sqr (- (drop-x d) f))
           (sqr (- (drop-y d) FIRE-Y)))))

; Drop List-of-fires -> Boolean
(check-expect (drop-collided-with-fire? (make-drop FIRE1 FIRE-Y) '()) #false)

(define (drop-collided-with-fire? d lof)
  (cond
    [(empty? lof) #false]
    [else (or (<= (distance d (first lof)) COL-RAD)
              (drop-collided-with-fire? d (rest lof)))]))

; Drop List-of-fires -> Boolean
; return #true if the drop d has collided with one of the fires in lof or with the ground

; collision with fire
(check-expect (drop-collided? (make-drop FIRE1 FIRE-Y) (list FIRE1)) #true)
(check-expect (drop-collided? (make-drop (+ FIRE1 COL-RAD) FIRE-Y) (list FIRE1)) #true)
(check-expect (drop-collided? (make-drop (- FIRE1 COL-RAD) FIRE-Y) (list FIRE1)) #true)
(check-expect (drop-collided? (make-drop FIRE1 (- FIRE-Y COL-RAD)) (list FIRE1)) #true)

; no collision with fire
(check-expect (drop-collided? (make-drop (+ FIRE1 COL-RAD 1) FIRE-Y) (list FIRE1)) #false)
(check-expect (drop-collided? (make-drop (- FIRE1 COL-RAD 1) FIRE-Y) (list FIRE1)) #false)
(check-expect (drop-collided? (make-drop FIRE1 (- FIRE-Y COL-RAD 1)) (list FIRE1)) #false)

; collision with the ground
(check-expect (drop-collided? (make-drop CENTER-X DROP-MAX-Y) '()) #true)
(check-expect (drop-collided? (make-drop CENTER-X (+ DROP-MAX-Y 1)) '()) #true)

; no collision with the ground
(check-expect (drop-collided? (make-drop CENTER-X (- DROP-MAX-Y 1)) '()) #false)

; multiple fires
(check-expect (drop-collided? (make-drop FIRE2 FIRE-Y) (list FIRE1 FIRE2)) #true)
(check-expect (drop-collided? (make-drop FIRE2 (- FIRE-Y COL-RAD 1)) (list FIRE1 FIRE2)) #false)

(define (drop-collided? d lof)
  (or (drop-collided-with-ground? d)
      (drop-collided-with-fire? d lof)))
      

; List-of-drops List-of-fires -> List-of-drops
; remove drops that collided with fires of with the ground

; no drops
(check-expect (remove-collided-drops '() '()) '())

; collision with fire
(check-expect (remove-collided-drops (list (make-drop FIRE1 FIRE-Y))
                                     (list FIRE1))
              '())
(check-expect (remove-collided-drops (list (make-drop (+ FIRE1 COL-RAD) FIRE-Y))
                                     (list FIRE1))
              '())
(check-expect (remove-collided-drops (list (make-drop (- FIRE1 COL-RAD) FIRE-Y))
                                     (list FIRE1))
              '())
(check-expect (remove-collided-drops (list (make-drop FIRE1 (- FIRE-Y COL-RAD)))
                                     (list FIRE1))
              '())

; no collision with fire
(check-expect (remove-collided-drops (list (make-drop (+ FIRE1 COL-RAD 1) FIRE-Y))
                                     (list FIRE1))
              (list (make-drop (+ FIRE1 COL-RAD 1) FIRE-Y)))
(check-expect (remove-collided-drops (list (make-drop (- FIRE1 COL-RAD 1) FIRE-Y))
                                     (list FIRE1))
              (list (make-drop (- FIRE1 COL-RAD 1) FIRE-Y)))
(check-expect (remove-collided-drops (list (make-drop FIRE1 (- FIRE-Y COL-RAD 1)))
                                     (list FIRE1))
              (list (make-drop FIRE1 (- FIRE-Y COL-RAD 1))))

; collision with the ground
(check-expect (remove-collided-drops (list (make-drop CENTER-X DROP-MAX-Y))
                                     '())
              '())

; no collision with the ground
(check-expect (remove-collided-drops (list (make-drop CENTER-X (- DROP-MAX-Y 1)))
                                     '())
              (list (make-drop FIRE1 (- DROP-MAX-Y 1))))

; multiple drops and fires
(check-expect (remove-collided-drops (list (make-drop FIRE1 FIRE-Y) (make-drop FIRE2 FIRE-Y))
                                     (list FIRE1 FIRE2))
              '())
(check-expect (remove-collided-drops (list (make-drop FIRE1 (- FIRE-Y COL-RAD 1)) (make-drop FIRE2 FIRE-Y))
                                     (list FIRE1 FIRE2))
              (list (make-drop FIRE1 (- FIRE-Y COL-RAD 1))))
(check-expect (remove-collided-drops (list (make-drop FIRE1 FIRE-Y) (make-drop FIRE2 (- FIRE-Y COL-RAD 1)))
                                     (list FIRE1 FIRE2))
              (list (make-drop FIRE2 (- FIRE-Y COL-RAD 1))))

(define (remove-collided-drops lod lof)
  (cond
    [(empty? lod) '()]
    [else (if (drop-collided? (first lod) lof)
              (remove-collided-drops (rest lod) lof)
              (cons (first lod) (remove-collided-drops (rest lod) lof)))]))

; Fire List-of-drops -> Boolean
; return #true when the fire f has collided with one of the drops in lod
(define (extinguished? f lod)
  (cond
    [(empty? lod) #false]
    [else (or (<= (distance (first lod) f) COL-RAD)
              (extinguished? f (rest lod)))]))

; List-of-drops List-of-fires -> List-of-fires
; extinguish fires that has collided with water drops
(check-expect (extinguish-fires '() '()) '())

; collision: 1 drop, 1 fire
(check-expect (extinguish-fires (list (make-drop FIRE1 FIRE-Y))
                                (list FIRE1))
              '())
(check-expect (extinguish-fires (list (make-drop (+ FIRE1 COL-RAD) FIRE-Y))
                                (list FIRE1))
              '())
(check-expect (extinguish-fires (list (make-drop (- FIRE1 COL-RAD) FIRE-Y))
                                (list FIRE1))
              '())
(check-expect (extinguish-fires (list (make-drop FIRE1 (- FIRE-Y COL-RAD)))
                                (list FIRE1))
              '())

; no collision: 1 drop, 1 fire
(check-expect (extinguish-fires (list (make-drop (+ FIRE1 COL-RAD 1) FIRE-Y))
                                (list FIRE1))
              (list FIRE1))
(check-expect (extinguish-fires (list (make-drop (- FIRE1 COL-RAD 1) FIRE-Y))
                                (list FIRE1))
              (list FIRE1))
(check-expect (extinguish-fires (list (make-drop FIRE1 (- FIRE-Y COL-RAD 1)))
                                (list FIRE1))
              (list FIRE1))

; collision: 1 drop, 2 fires
(check-expect (extinguish-fires (list (make-drop FIRE2 FIRE-Y))
                                (list FIRE1 FIRE2))
              (list FIRE1))
(check-expect (extinguish-fires (list (make-drop FIRE1 FIRE-Y))
                                (list FIRE1 FIRE2))
              (list FIRE2))

; water hits multiple fires
(check-expect (extinguish-fires (list (make-drop FIRE1 FIRE-Y))
                                (list FIRE1 (+ FIRE1 COL-RAD)))
              '())

; collision: 2 drops, 2 fires
(check-expect (extinguish-fires (list (make-drop FIRE1 FIRE-Y) (make-drop FIRE2 FIRE-Y))
                                (list FIRE1 FIRE2))
              '())

(define (extinguish-fires lod lof)
  (cond
    [(empty? lof) '()]
    [else (if (extinguished? (first lof) lod)
              (extinguish-fires lod (rest lof))
              (cons (first lof) (extinguish-fires lod (rest lof))))]))

; Plane -> Plane
; move the plane for one tick

; move right
(check-expect (move-plane (make-plane CENTER-X "right"))
              (make-plane (+ CENTER-X PLANE-SPEED) "right"))

; move left
(check-expect (move-plane (make-plane CENTER-X "left"))
              (make-plane (- CENTER-X PLANE-SPEED) "left"))

; hit right wall
(check-expect (move-plane (make-plane PLANE-MAX-X "right"))
              (make-plane PLANE-MAX-X "right"))

; hit left wall
(check-expect (move-plane (make-plane PLANE-MIN-X "left"))
              (make-plane PLANE-MIN-X "left"))

(define (move-plane p)
  (cond
    [(string=? (plane-dir p) "left")
     (make-plane (max (- (plane-x p) PLANE-SPEED) PLANE-MIN-X) "left")]
    [(string=? (plane-dir p) "right")
     (make-plane (min (+ (plane-x p) PLANE-SPEED) PLANE-MAX-X) "right")]))

; Drop -> Drop
(define (move-drop d)
  (make-drop (drop-x d) (+ (drop-y d) DROP-SPEED)))

; List-of-drops -> List-of-drops
; move water drops down for one tick
(check-expect (move-drops '()) '())
(check-expect (move-drops (list (make-drop 0 0) (make-drop 20 30)))
              (list (make-drop 0 DROP-SPEED) (make-drop 20 (+ 30 DROP-SPEED))))

(define (move-drops lod)
  (cond
    [(empty? lod) '()]
    [else (cons (move-drop (first lod)) (move-drops (rest lod)))]))

; List-of-fires N -> List-of-fires
; add new random fire every TICKS-PER-FIRE ticks
(check-random (add-fire '() 0) (list (random WIDTH)))
(check-random (add-fire '() TICKS-PER-FIRE) (list (random WIDTH)))
(check-expect (add-fire '() (+ TICKS-PER-FIRE 1)) '())
(check-random (add-fire '() (* TICKS-PER-FIRE 2)) (list (random WIDTH)))
(check-random (add-fire (list FIRE1) TICKS-PER-FIRE) (list (random WIDTH) FIRE1))

(define (add-fire lof ticks)
  (if (zero? (modulo ticks TICKS-PER-FIRE))
      (cons (random WIDTH) lof)
      lof))

; Game -> Game
; update the game for one tick
(check-expect (update-game (make-game (make-plane CENTER-X "right") '() '() 1))
              (make-game (make-plane (+ CENTER-X PLANE-SPEED) "right") '() '() 2))

(define (update-game g)
  (make-game (move-plane (game-plane g))
             (remove-collided-drops (move-drops (game-drops g))
                                    (add-fire (game-fires g) (game-ticks g)))
             (extinguish-fires (move-drops (game-drops g))
                               (add-fire (game-fires g) (game-ticks g)))
             (add1 (game-ticks g))))

; Plane Direction -> Plane
(define (turn-plane p dir)
  (make-plane (plane-x p) dir))

; Plane -> Drop
(define (new-drop p)
  (make-drop (plane-x p) DROP-MIN-Y))

; Game KeyEvent -> Game
(check-expect (handle-keys (make-game (make-plane CENTER-X "right") '() '() 1) " ")
              (make-game (make-plane CENTER-X "right") (list (make-drop CENTER-X DROP-MIN-Y)) '() 1))
(check-expect (handle-keys (make-game (make-plane CENTER-X "right") '() '() 1) "left")
              (make-game (make-plane CENTER-X "left") '() '() 1))
(check-expect (handle-keys (make-game (make-plane CENTER-X "left") '() '() 1) "right")
              (make-game (make-plane CENTER-X "right") '() '() 1))
(check-expect (handle-keys (make-game (make-plane CENTER-X "left") '() '() 1) "a")
              (make-game (make-plane CENTER-X "left") '() '() 1))

(define (handle-keys g ke)
  (cond
    [(key=? ke "left") (make-game (turn-plane (game-plane g) "left")
                                  (game-drops g)
                                  (game-fires g)
                                  (game-ticks g))]
    [(key=? ke "right") (make-game (turn-plane (game-plane g) "right")
                                   (game-drops g)
                                   (game-fires g)
                                   (game-ticks g))]
    [(key=? ke " ") (make-game (game-plane g)
                               (cons (new-drop (game-plane g)) (game-drops g))
                               (game-fires g)
                               (game-ticks g))]
    [else g]))

; Number -> Game
; start the game with the given clock rate: (main 1/60)
(define (main rate)
  (big-bang (make-game (make-plane CENTER-X "right") '() (list (random WIDTH)) 1)
    [to-draw render-game]
    [on-tick update-game rate]
    [on-key handle-keys]
    [stop-when game-over? render-game]))
