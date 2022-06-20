;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |218|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct world [worm collision])
; A World is a structure:
;   (make-world Worm Collision)

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
  (cond
    [(empty? los) img]
    [else (render-segment (first los) (render-segments (rest los) img))]))

; Worm -> Image
; render the worm
(check-expect (render-worm (make-worm (list (make-posn 0 0)
                                            (make-posn 1 0)
                                            (make-posn 1 1)
                                            (make-posn 2 1)) "up"))
              (place-image SEG SEG-R SEG-R
                           (place-image SEG (+ SEG-R CELL-SIZE) SEG-R
                                        (place-image SEG (+ SEG-R CELL-SIZE) (+ SEG-R CELL-SIZE)
                                                     (place-image SEG (+ SEG-R (* 2 CELL-SIZE)) (+ SEG-R CELL-SIZE)
                                                                  MTS)))))
(define (render-worm w)
  (render-segments (worm-seg w) MTS))

; World -> Image
; render the game
(check-expect (render (make-world W1 #false))
              (render-worm W1))

(define (render w)
  (render-worm (world-worm w)))

; World -> Image
; render the game over screen
(check-expect (render-game-over (make-world W1 "wall"))
              (overlay/align "left" "bottom"
                             (text "worm hit border" FONT-SIZE FONT-COLOR)
                             (render-worm W1)))
(check-expect (render-game-over (make-world W1 "worm"))
              (overlay/align "left" "bottom"
                             (text "worm hit itself" FONT-SIZE FONT-COLOR)
                             (render-worm W1)))

(define (render-game-over w)
  (overlay/align "left" "bottom"
                 (text (cond
                         [(string=? (world-collision w) "wall") "worm hit border"]
                         [(string=? (world-collision w) "worm") "worm hit itself"])
                       FONT-SIZE
                       FONT-COLOR)
                 (render-worm (world-worm w))))

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
    [(<= (length l) 1) '()]
    [else (cons (first l) (remove-last (rest l)))]))

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

; Posn Posn -> Boolean
; return #true if two positions are equal
(check-expect (posn=? (make-posn 0 0) (make-posn 0 0)) #true)
(check-expect (posn=? (make-posn 2 3) (make-posn 2 3)) #true)
(check-expect (posn=? (make-posn 1 0) (make-posn 0 0)) #false)
(check-expect (posn=? (make-posn 0 1) (make-posn 0 0)) #false)
(check-expect (posn=? (make-posn 1 1) (make-posn 0 0)) #false)

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

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

; World -> World
; update the game for one tick
(check-expect (update (make-world (make-worm (list (make-posn 0 0)) "left") #false))
              (make-world (make-worm (list (make-posn 0 0)) "left") "wall"))
(check-expect (update (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0)) "left") #false))
              (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0)) "left") "worm"))
(check-expect (update (make-world (make-worm (list (make-posn 1 0) (make-posn 0 0) (make-posn 0 1)) "right") #false))
              (make-world (make-worm (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) "right") #false))

(define (update w)
  (if (false? (collision (world-worm w)))
      (make-world (move-worm (world-worm w)) #false)
      (make-world (world-worm w) (collision (world-worm w)))))

; World KeyEvent -> World
; handle key events
(check-expect (handle-keys (make-world (make-worm '() "up") #false) "right")
              (make-world (make-worm '() "right") #false))
(check-expect (handle-keys (make-world (make-worm '() "up") #false) "left")
              (make-world (make-worm '() "left") #false))
(check-expect (handle-keys (make-world (make-worm '() "left") #false) "up")
              (make-world (make-worm '() "up") #false))
(check-expect (handle-keys (make-world (make-worm '() "left") #false) "down")
              (make-world (make-worm '() "down") #false))
(check-expect (handle-keys (make-world (make-worm '() "left") #false) "a")
              (make-world (make-worm '() "left") #false))

(define (handle-keys w k)
  (if (member? k (list "left" "right" "up" "down"))
      (make-world (make-worm (worm-seg (world-worm w)) k) (world-collision w))
      w))

; World -> Boolean
; return #true when the game is over
(check-expect (game-over? (make-world W1 #false)) #false)
(check-expect (game-over? (make-world W1 "wall")) #true)
(check-expect (game-over? (make-world W1 "worm")) #true)

(define (game-over? w)
  (not (false? (world-collision w))))

; Number -> World
; run the program with the given clock rate
(define (worm-main rate)
  (big-bang (make-world (make-worm (list (make-posn 5 5)
                                         (make-posn 6 5)
                                         (make-posn 6 6)
                                         (make-posn 7 6)
                                         (make-posn 7 7)
                                         (make-posn 8 7)
                                         (make-posn 9 7)) "left")
                        #false)
    [to-draw render]
    [on-tick update rate]
    [on-key handle-keys]
    [stop-when game-over? render-game-over]))
