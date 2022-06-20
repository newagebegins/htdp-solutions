;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |216|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define WORM-RADIUS (/ CELL-SIZE 2))
(define FONT-SIZE 20)
(define FONT-COLOR "black")

(define WORM (circle WORM-RADIUS "solid" "red"))
(define MTS (empty-scene WIDTH HEIGHT))

(define-struct worm [x y dx dy hit-wall?])
; A Worm is a structure:
;   (make-worm Number Number Number Number Boolean)
; interp. x and y are the column and row coordinates of the worm on the playing field.
;         dx and dy are the speed in cells per tick.
;         hit-wall? is #true when the worm just tried to move into a wall.

; Worm -> Image
; render the game
(check-expect (render (make-worm 0 0 0 0 #false))
              (place-image WORM WORM-RADIUS WORM-RADIUS MTS))
(check-expect (render (make-worm 1 0 0 0 #false))
              (place-image WORM (+ WORM-RADIUS CELL-SIZE) WORM-RADIUS MTS))
(check-expect (render (make-worm 0 1 0 0 #false))
              (place-image WORM WORM-RADIUS (+ WORM-RADIUS CELL-SIZE) MTS))
(check-expect (render (make-worm 2 0 0 0 #false))
              (place-image WORM (+ WORM-RADIUS (* 2 CELL-SIZE)) WORM-RADIUS MTS))
(check-expect (render (make-worm 2 4 0 0 #false))
              (place-image WORM (+ WORM-RADIUS (* 2 CELL-SIZE)) (+ WORM-RADIUS (* 4 CELL-SIZE)) MTS))

(define (render w)
  (place-image WORM (+ WORM-RADIUS (* (worm-x w) CELL-SIZE)) (+ WORM-RADIUS (* (worm-y w) CELL-SIZE)) MTS))

; Worm -> Worm
; move the worm for one tick
(check-expect (move-worm (make-worm 0 0 -1 0 #false)) (make-worm -1 0 -1 0 #false))
(check-expect (move-worm (make-worm (- COLUMNS 1) 0 1 0 #false)) (make-worm COLUMNS 0 1 0 #false))
(check-expect (move-worm (make-worm 0 0 0 -1 #false)) (make-worm 0 -1 0 -1 #false))
(check-expect (move-worm (make-worm 0 (- ROWS 1) 0 1 #false)) (make-worm 0 ROWS 0 1 #false))
(check-expect (move-worm (make-worm 0 0 0 -1 #true)) (make-worm 0 -1 0 -1 #true))

(define (move-worm w)
  (make-worm (+ (worm-x w) (worm-dx w))
             (+ (worm-y w) (worm-dy w))
             (worm-dx w)
             (worm-dy w)
             (worm-hit-wall? w)))

; Worm -> Boolean
; return #true when the worm has moved beyond the playfield
(check-expect (out-of-bounds? (make-worm -1 0 -1 0 #false)) #true)
(check-expect (out-of-bounds? (make-worm COLUMNS 0 1 0 #false)) #true)
(check-expect (out-of-bounds? (make-worm (+ COLUMNS 1) 0 1 0 #false)) #true)
(check-expect (out-of-bounds? (make-worm 0 -1 0 -1 #false)) #true)
(check-expect (out-of-bounds? (make-worm 0 ROWS 0 1 #false)) #true)
(check-expect (out-of-bounds? (make-worm 0 (+ ROWS 1) 0 1 #false)) #true)

(define (out-of-bounds? w)
  (or (< (worm-x w) 0)
      (< (worm-y w) 0)
      (>= (worm-x w) COLUMNS)
      (>= (worm-y w) ROWS)))

; Worm -> Worm
; update the game
(check-expect (update (make-worm 0 0 1 0 #false)) (make-worm 1 0 1 0 #false))
(check-expect (update (make-worm 0 0 0 1 #false)) (make-worm 0 1 0 1 #false))
(check-expect (update (make-worm 1 0 -1 0 #false)) (make-worm 0 0 -1 0 #false))
(check-expect (update (make-worm 0 1 0 -1 #false)) (make-worm 0 0 0 -1 #false))
(check-expect (update (make-worm 5 2 1 0 #false)) (make-worm 6 2 1 0 #false))

(check-expect (update (make-worm 0 0 -1 0 #false)) (make-worm 0 0 -1 0 #true))
(check-expect (update (make-worm (- COLUMNS 1) 0 1 0 #false)) (make-worm (- COLUMNS 1) 0 1 0 #true))
(check-expect (update (make-worm 0 0 0 -1 #false)) (make-worm 0 0 0 -1 #true))
(check-expect (update (make-worm 0 (- ROWS 1) 0 1 #false)) (make-worm 0 (- ROWS 1) 0 1 #true))

(define (update w)
  (if (out-of-bounds? (move-worm w))
      (make-worm (worm-x w) (worm-y w) (worm-dx w) (worm-dy w) #true)
      (move-worm w)))

; Worm KeyEvent -> Worm
; handle key events
(check-expect (handle-keys (make-worm 0 0 -1  0 #false) "right") (make-worm 0 0 1 0 #false))
(check-expect (handle-keys (make-worm 0 0  1  0 #false) "right") (make-worm 0 0 1 0 #false))
(check-expect (handle-keys (make-worm 0 0  0  1 #false) "right") (make-worm 0 0 1 0 #false))
(check-expect (handle-keys (make-worm 0 0  0 -1 #true) "right") (make-worm 0 0 1 0 #true))

(check-expect (handle-keys (make-worm 1 2 -1  0 #false) "left") (make-worm 1 2 -1 0 #false))
(check-expect (handle-keys (make-worm 1 2  1  0 #false) "left") (make-worm 1 2 -1 0 #false))
(check-expect (handle-keys (make-worm 1 2  0  1 #false) "left") (make-worm 1 2 -1 0 #false))
(check-expect (handle-keys (make-worm 1 2  0 -1 #true) "left") (make-worm 1 2 -1 0 #true))

(check-expect (handle-keys (make-worm 0 0 -1  0 #false) "up") (make-worm 0 0 0 -1 #false))
(check-expect (handle-keys (make-worm 0 0  1  0 #false) "up") (make-worm 0 0 0 -1 #false))
(check-expect (handle-keys (make-worm 0 0  0  1 #false) "up") (make-worm 0 0 0 -1 #false))
(check-expect (handle-keys (make-worm 0 0  0 -1 #true) "up") (make-worm 0 0 0 -1 #true))

(check-expect (handle-keys (make-worm 0 0 -1  0 #false) "down") (make-worm 0 0 0 1 #false))
(check-expect (handle-keys (make-worm 0 0  1  0 #false) "down") (make-worm 0 0 0 1 #false))
(check-expect (handle-keys (make-worm 0 0  0  1 #false) "down") (make-worm 0 0 0 1 #false))
(check-expect (handle-keys (make-worm 0 0  0 -1 #true) "down") (make-worm 0 0 0 1 #true))

(check-expect (handle-keys (make-worm 1 2 1 0 #false) "a") (make-worm 1 2 1 0 #false))
(check-expect (handle-keys (make-worm 3 4 0 1 #true) " ") (make-worm 3 4 0 1 #true))

(define (handle-keys w k)
  (cond
    [(key=? k "left") (make-worm (worm-x w) (worm-y w) -1 0 (worm-hit-wall? w))]
    [(key=? k "right") (make-worm (worm-x w) (worm-y w) 1 0 (worm-hit-wall? w))]
    [(key=? k "up") (make-worm (worm-x w) (worm-y w) 0 -1 (worm-hit-wall? w))]
    [(key=? k "down") (make-worm (worm-x w) (worm-y w) 0 1 (worm-hit-wall? w))]
    [else w]))

; Worm -> Boolean
; return #true if the worm hit a wall
(check-expect (hit-wall? (make-worm 0 0 1 0 #true)) #true)
(check-expect (hit-wall? (make-worm 0 0 1 0 #false)) #false)

(define (hit-wall? w)
  (worm-hit-wall? w))

; Worm -> Image
; render the game over scene
(check-expect (render-game-over (make-worm 0 0 -1 0 #true))
              (overlay/align "left" "bottom"
                             (text "worm hit border" FONT-SIZE FONT-COLOR)
                             (render (make-worm 0 0 -1 0 #true))))

(define (render-game-over w)
  (overlay/align "left" "bottom"
                 (text "worm hit border" FONT-SIZE FONT-COLOR)
                 (render w)))

; Number -> Worm
; run the program with the given clock rate
(define (worm-main rate)
  (big-bang (make-worm 5 5 1 0 #false)
    [to-draw render]
    [on-tick update rate]
    [on-key handle-keys]
    [stop-when hit-wall? render-game-over]))
