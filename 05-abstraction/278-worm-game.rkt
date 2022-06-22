;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 278-worm-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; logical dimensions of the playing field
(define COLUMNS 15)
(define ROWS 10)

; size of one cell in pixels
(define CELL-SIZE 20)

; pixel dimensions of the playing field
(define WIDTH (* COLUMNS CELL-SIZE))
(define HEIGHT (* ROWS CELL-SIZE))

(define SEG-R (/ CELL-SIZE 2))                       ; segment radius
(define SEG (circle (/ CELL-SIZE 2) "solid" "red"))  ; segment
(define FOOD (circle (/ CELL-SIZE 2) "solid" "green"))
(define MTS (empty-scene WIDTH HEIGHT))              ; empty scene

(define FONT-SIZE 20)
(define FONT-COLOR "black")

; A Segment is a Posn:
;   (make-posn Integer Integer)
; interp. represents a position of a worm segment on the playfield

; A List-of-segments is one of:
; - '()
; - (cons Segment List-of-segments)
; interp. a list of worm segments, the first segment represents the head

; A Direction is one of:
; - "right"
; - "left"
; - "up"
; - "down"
; interp. the direction of movement of the worm's head

(define-struct worm [seg dir])
; A Worm is a structure:
;   (make-worm List-of-segments Direction)
; interp. the worm consisting of segments, it's head moving in the given direction

; A Collision is one of:
; - "wall"
; - "worm"
; - #false
; interp. determines an object that the worm has collided with, or #false if no collision has occured yet

(define-struct world [worm food collision])
; A World is a structure:
;   (make-world Worm Posn Collision)

(define F1 (make-posn 8 8))
(define W1 (make-worm (list (make-posn 0 0)) "up"))

; Segment Image -> Image
; render a segment onto the given image
(check-expect (render-segment (make-posn 2 3) MTS)
              (place-image SEG
                           (+ SEG-R (* 2 CELL-SIZE))
                           (+ SEG-R (* 3 CELL-SIZE))
                           MTS))

(define (render-segment s img)
  (place-image SEG
               (+ SEG-R (* (posn-x s) CELL-SIZE))
               (+ SEG-R (* (posn-y s) CELL-SIZE))
               img))

; List-of-segments Image -> Image
; render segments onto the given image
(check-expect (render-segments '() MTS) MTS)
(check-expect (render-segments (list (make-posn 0 0)) MTS)
              (place-image SEG SEG-R SEG-R MTS))
(check-expect (render-segments (list (make-posn 0 0)
                                     (make-posn 1 0)
                                     (make-posn 1 1)
                                     (make-posn 2 1)) MTS)
              (place-image SEG SEG-R SEG-R
                           (place-image SEG (+ SEG-R CELL-SIZE) SEG-R
                                        (place-image SEG (+ SEG-R CELL-SIZE) (+ SEG-R CELL-SIZE)
                                                     (place-image SEG (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R CELL-SIZE)
                                                                  MTS)))))

(define (render-segments los img)
  (foldr render-segment img los))

; Worm -> Image
; render the worm
(check-expect (render-worm (make-worm (list (make-posn 0 0)
                                            (make-posn 1 0)
                                            (make-posn 1 1)
                                            (make-posn 2 1)) "up")
                           MTS)
              (place-image SEG SEG-R SEG-R
                           (place-image SEG (+ SEG-R CELL-SIZE) SEG-R
                                        (place-image SEG (+ SEG-R CELL-SIZE) (+ SEG-R CELL-SIZE)
                                                     (place-image SEG (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R CELL-SIZE)
                                                                  MTS)))))
(define (render-worm w img)
  (render-segments (worm-seg w) img))

; Posn Image -> Image
(check-expect (render-food (make-posn 2 1) MTS)
              (place-image FOOD (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R (* 1 CELL-SIZE)) MTS))

(define (render-food f img)
  (place-image FOOD (+ SEG-R (* (posn-x f) CELL-SIZE)) (+ SEG-R (* (posn-y f) CELL-SIZE)) img))

; World -> Image
; render the game
(define (render w)
  (render-worm (world-worm w)
               (render-food (world-food w) MTS)))

; World -> Image
; render the game over screen
(check-expect (render-game-over (make-world W1 F1 "wall"))
              (overlay/align "left" "bottom"
                             (text "worm hit border" FONT-SIZE FONT-COLOR)
                             (render (make-world W1 F1 "wall"))))
(check-expect (render-game-over (make-world W1 F1 "worm"))
              (overlay/align "left" "bottom"
                             (text "worm hit itself" FONT-SIZE FONT-COLOR)
                             (render (make-world W1 F1 "worm"))))

(define (render-game-over w)
  (overlay/align "left" "bottom"
                 (text (cond
                         [(string=? (world-collision w) "wall") "worm hit border"]
                         [(string=? (world-collision w) "worm") "worm hit itself"])
                       FONT-SIZE
                       FONT-COLOR)
                 (render w)))

; Segment Direction -> Segment
; move the head segment to the new position in the given direction
(check-expect (move-head (make-posn 0 3) "right") (make-posn 1 3))
(check-expect (move-head (make-posn 0 3) "left") (make-posn -1 3))
(check-expect (move-head (make-posn 0 3) "up") (make-posn 0 2))
(check-expect (move-head (make-posn 0 3) "down") (make-posn 0 4))

(define (move-head head dir)
  (cond
    [(string=? dir "right") (make-posn (add1 (posn-x head)) (posn-y head))]
    [(string=? dir "left") (make-posn (sub1 (posn-x head)) (posn-y head))]
    [(string=? dir "up") (make-posn (posn-x head) (sub1 (posn-y head)))]
    [(string=? dir "down") (make-posn (posn-x head) (add1 (posn-y head)))]))

; List -> List
; remove the last element of the list
(check-expect (remove-last '()) '())
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))
(check-expect (remove-last (list 1 2 3)) (list 1 2))

(define (remove-last l)
  (cond
    [(empty? l) '()]
    [else (reverse (rest (reverse l)))]))

; Segment -> Boolean
; return #true if the head is outside the playing field
(check-expect (out-of-bounds? (make-posn -1 0)) #true)
(check-expect (out-of-bounds? (make-posn 0 -1)) #true)
(check-expect (out-of-bounds? (make-posn COLUMNS 0)) #true)
(check-expect (out-of-bounds? (make-posn 0 ROWS)) #true)
(check-expect (out-of-bounds? (make-posn 0 0)) #false)

(define (out-of-bounds? h)
  (or (< (posn-x h) 0)
      (< (posn-y h) 0)
      (>= (posn-x h) COLUMNS)
      (>= (posn-y h) ROWS)))

; Segment List-of-segments -> Boolean
; return #true if the head coincides with one of the segments in tail
(check-expect (hit-itself? (make-posn 0 0) '()) #false)
(check-expect (hit-itself? (make-posn 0 0) (list (make-posn 0 0))) #true)
(check-expect (hit-itself? (make-posn 1 1) (list (make-posn 0 1) (make-posn 1 1) (make-posn 2 1))) #true)
(check-expect (hit-itself? (make-posn 2 2) (list (make-posn 0 1) (make-posn 1 1) (make-posn 2 1))) #false)

(define (hit-itself? h tail)
  (member? h tail))

; Worm -> Collision
; check if a collision occurs for the next worm move
(check-expect (collision (make-worm (list (make-posn 0 0)) "left"))
              "wall")
(check-expect (collision (make-worm (list (make-posn 0 0)) "up"))
              "wall")
(check-expect (collision (make-worm (list (make-posn (- COLUMNS 1) 0)) "right"))
              "wall")
(check-expect (collision (make-worm (list (make-posn 0 (- ROWS 1))) "down"))
              "wall")

(check-expect (collision (make-worm (list (make-posn 1 0) (make-posn 0 0)) "left"))
              "worm")
(check-expect (collision (make-worm (list (make-posn 0 0) (make-posn 1 0)) "right"))
              "worm")
(check-expect (collision (make-worm (list (make-posn 0 1) (make-posn 0 0)) "up"))
              "worm")
(check-expect (collision (make-worm (list (make-posn 0 0) (make-posn 0 1)) "down"))
              "worm")

(check-expect (collision (make-worm (list (make-posn 1 1)) "left"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1)) "right"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1)) "up"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1)) "down"))
              #false)

(check-expect (collision (make-worm (list (make-posn 1 1) (make-posn 2 1)) "left"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1) (make-posn 0 1)) "right"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1) (make-posn 2 1)) "up"))
              #false)
(check-expect (collision (make-worm (list (make-posn 1 1) (make-posn 1 0)) "down"))
              #false)

(define (collision w)
  (cond
    [(out-of-bounds? (move-head (first (worm-seg w)) (worm-dir w))) "wall"]
    [(hit-itself? (move-head (first (worm-seg w)) (worm-dir w)) (rest (worm-seg w))) "worm"]
    [else #false]))

; Worm -> Worm
; move the worm for one tick
(check-expect (move-worm (make-worm (list (make-posn 5 3) (make-posn 4 3) (make-posn 4 2)) "right"))
              (make-worm (list (make-posn 6 3) (make-posn 5 3) (make-posn 4 3)) "right"))
(check-expect (move-worm (make-worm (list (make-posn 5 5) (make-posn 5 4) (make-posn 6 4)) "left"))
              (make-worm (list (make-posn 4 5) (make-posn 5 5) (make-posn 5 4)) "left"))

(define (move-worm w)
  (make-worm (cons (move-head (first (worm-seg w)) (worm-dir w))
                   (remove-last (worm-seg w)))
             (worm-dir w)))

; Worm -> Worm
; grow the worm after eating food
(check-expect (eat-food (make-worm (list (make-posn 0 0)) "right"))
              (make-worm (list (make-posn 1 0) (make-posn 0 0)) "right"))

(define (eat-food w)
  (make-worm (cons (move-head (first (worm-seg w)) (worm-dir w))
                   (worm-seg w))
             (worm-dir w)))

; List -> Any
; returns random element of the list
(check-expect (select-random (list 8)) 8)
(check-member-of (select-random (list 1 2 3)) 1 2 3)

(define (select-random l)
  (list-ref l (random (length l))))

; N N -> List-of-posns
; create a list of all the cells on the playfield
(check-expect (all-cells 0 0) '())
(check-expect (all-cells 1 0) '())
(check-expect (all-cells 1 1) (list (make-posn 0 0)))
(check-expect (all-cells 2 1) (list (make-posn 0 0) (make-posn 1 0)))
(check-expect (all-cells 1 2) (list (make-posn 0 0) (make-posn 0 1)))
(check-expect (all-cells 2 2) (list (make-posn 0 0) (make-posn 1 0)
                                    (make-posn 0 1) (make-posn 1 1)))
(check-expect (all-cells 3 3) (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)
                                    (make-posn 0 1) (make-posn 1 1) (make-posn 2 1)
                                    (make-posn 0 2) (make-posn 1 2) (make-posn 2 2)))
(check-expect (all-cells 3 5) (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)
                                    (make-posn 0 1) (make-posn 1 1) (make-posn 2 1)
                                    (make-posn 0 2) (make-posn 1 2) (make-posn 2 2)
                                    (make-posn 0 3) (make-posn 1 3) (make-posn 2 3)
                                    (make-posn 0 4) (make-posn 1 4) (make-posn 2 4)))

(define (all-cells col# row#)
  (local (; N -> Posn
          (define (f i)
            (make-posn (modulo i col#) (quotient i col#))))
    (build-list (* col# row#) f)))

; List-of-posns List-of-posns -> List-of-posns
; given lists of all cells and the cells with worm segments create the list of all empty cells
(check-expect (empty-cells (list (make-posn 0 0)) '())
              (list (make-posn 0 0)))
(check-expect (empty-cells (list (make-posn 0 0)) (list (make-posn 0 0)))
              '())
(check-expect (empty-cells (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)
                                 (make-posn 0 1) (make-posn 1 1) (make-posn 2 1))
                           (list (make-posn 0 1) (make-posn 1 1) (make-posn 1 0)))
              (list (make-posn 0 0) (make-posn 2 0) (make-posn 2 1)))

(define (empty-cells all seg)
  (foldr remove all seg))

; List-of-segments -> Posn
; generate random food position
; the food must not occupy the same cell as one of the worm segments
(define (random-food seg)
  (select-random (empty-cells (all-cells COLUMNS ROWS) seg)))

; World -> Boolean
; return #true if on the next move the worm will occupy the same cell as the food
(check-expect (eat-food? (make-world (make-worm (list (make-posn 0 0)) "right")
                                     (make-posn 1 0)
                                     #false))
              #true)
(check-expect (eat-food? (make-world (make-worm (list (make-posn 0 0)) "right")
                                     (make-posn 1 1)
                                     #false))
              #false)

(define (eat-food? w)
  (equal? (move-head (first (worm-seg (world-worm w))) (worm-dir (world-worm w)))
          (world-food w)))

; World -> World
; update the game for one tick
(check-expect (update (make-world (make-worm (list (make-posn 0 0)) "left") F1 #false))
              (make-world (make-worm (list (make-posn 0 0)) "left") F1 "wall"))
(check-expect (update (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0)) "left") F1 #false))
              (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0)) "left") F1 "worm"))
(check-expect (update (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0) (make-posn 0 1)) "right") F1 #false))
              (make-world (make-worm (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) "right") F1 #false))

; eating the food
(check-random (update (make-world (make-worm (list (make-posn 0 0)) "right")
                                  (make-posn 1 0)
                                  #false))
              (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0)) "right")
                          (random-food (list (make-posn 1 0) (make-posn 0 0)))
                          #false))

(define (update w)
  (cond
    [(eat-food? w)
     (make-world (eat-food (world-worm w))
                 (random-food (worm-seg (eat-food (world-worm w))))
                 #false)]
    [(false? (collision (world-worm w)))
     (make-world (move-worm (world-worm w)) (world-food w) #false)]
    [else (make-world (world-worm w) (world-food w) (collision (world-worm w)))]))

; World KeyEvent -> World
; handle key events
(check-expect (handle-keys (make-world (make-worm '() "up") F1 #false) "right")
              (make-world (make-worm '() "right") F1 #false))
(check-expect (handle-keys (make-world (make-worm '() "up") F1 #false) "left")
              (make-world (make-worm '() "left") F1 #false))
(check-expect (handle-keys (make-world (make-worm '() "left") F1 #false) "up")
              (make-world (make-worm '() "up") F1 #false))
(check-expect (handle-keys (make-world (make-worm '() "left") F1 #false) "down")
              (make-world (make-worm '() "down") F1 #false))
(check-expect (handle-keys (make-world (make-worm '() "left") F1 #false) "a")
              (make-world (make-worm '() "left") F1 #false))

(define (handle-keys w k)
  (if (member? k (list "left" "right" "up" "down"))
      (make-world (make-worm (worm-seg (world-worm w)) k)
                  (world-food w)
                  (world-collision w))
      w))

; World -> Boolean
; return #true when the game is over
(check-expect (game-over? (make-world W1 F1 #false)) #false)
(check-expect (game-over? (make-world W1 F1 "wall")) #true)
(check-expect (game-over? (make-world W1 F1 "worm")) #true)

(define (game-over? w)
  (not (false? (world-collision w))))

; Number -> World
; run the program with the given clock rate, for example: (worm-main 1/5)
(define (worm-main rate)
  (big-bang (make-world (make-worm (list (make-posn 0 0)) "right")
                        (make-posn 5 5)
                        #false)
    [to-draw render]
    [on-tick update rate]
    [on-key handle-keys]
    [stop-when game-over? render-game-over]
    [state #false]))
