;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |215|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define WORM (circle WORM-RADIUS "solid" "red"))
(define MTS (empty-scene WIDTH HEIGHT))

(define-struct worm [x y dx dy])
; A Worm is a structure:
;   (make-worm Number Number Number Number)
; interp. x and y are the column and row coordinates of the worm on the playing field.
;         dx and dy are the speed in cells per tick.

; Worm -> Image
; render the game
(check-expect (render (make-worm 0 0 0 0))
              (place-image WORM WORM-RADIUS WORM-RADIUS MTS))
(check-expect (render (make-worm 1 0 0 0))
              (place-image WORM (+ WORM-RADIUS CELL-SIZE) WORM-RADIUS MTS))
(check-expect (render (make-worm 0 1 0 0))
              (place-image WORM WORM-RADIUS (+ WORM-RADIUS CELL-SIZE) MTS))
(check-expect (render (make-worm 2 0 0 0))
              (place-image WORM (+ WORM-RADIUS (* 2 CELL-SIZE)) WORM-RADIUS MTS))
(check-expect (render (make-worm 2 4 0 0))
              (place-image WORM (+ WORM-RADIUS (* 2 CELL-SIZE)) (+ WORM-RADIUS (* 4 CELL-SIZE)) MTS))

(define (render w)
  (place-image WORM (+ WORM-RADIUS (* (worm-x w) CELL-SIZE)) (+ WORM-RADIUS (* (worm-y w) CELL-SIZE)) MTS))

; Worm -> Worm
; update the game
(check-expect (update (make-worm 0 0 1 0)) (make-worm 1 0 1 0))
(check-expect (update (make-worm 0 0 0 1)) (make-worm 0 1 0 1))
(check-expect (update (make-worm 0 0 -1 0)) (make-worm -1 0 -1 0))
(check-expect (update (make-worm 0 0 0 -1)) (make-worm 0 -1 0 -1))
(check-expect (update (make-worm 5 2 1 0)) (make-worm 6 2 1 0))

(define (update w)
  (make-worm (+ (worm-x w) (worm-dx w))
             (+ (worm-y w) (worm-dy w))
             (worm-dx w)
             (worm-dy w)))

; Worm KeyEvent -> Worm
; handle key events
(check-expect (handle-keys (make-worm 0 0 -1  0) "right") (make-worm 0 0 1 0))
(check-expect (handle-keys (make-worm 0 0  1  0) "right") (make-worm 0 0 1 0))
(check-expect (handle-keys (make-worm 0 0  0  1) "right") (make-worm 0 0 1 0))
(check-expect (handle-keys (make-worm 0 0  0 -1) "right") (make-worm 0 0 1 0))

(check-expect (handle-keys (make-worm 1 2 -1  0) "left") (make-worm 1 2 -1 0))
(check-expect (handle-keys (make-worm 1 2  1  0) "left") (make-worm 1 2 -1 0))
(check-expect (handle-keys (make-worm 1 2  0  1) "left") (make-worm 1 2 -1 0))
(check-expect (handle-keys (make-worm 1 2  0 -1) "left") (make-worm 1 2 -1 0))

(check-expect (handle-keys (make-worm 0 0 -1  0) "up") (make-worm 0 0 0 -1))
(check-expect (handle-keys (make-worm 0 0  1  0) "up") (make-worm 0 0 0 -1))
(check-expect (handle-keys (make-worm 0 0  0  1) "up") (make-worm 0 0 0 -1))
(check-expect (handle-keys (make-worm 0 0  0 -1) "up") (make-worm 0 0 0 -1))

(check-expect (handle-keys (make-worm 0 0 -1  0) "down") (make-worm 0 0 0 1))
(check-expect (handle-keys (make-worm 0 0  1  0) "down") (make-worm 0 0 0 1))
(check-expect (handle-keys (make-worm 0 0  0  1) "down") (make-worm 0 0 0 1))
(check-expect (handle-keys (make-worm 0 0  0 -1) "down") (make-worm 0 0 0 1))

(check-expect (handle-keys (make-worm 1 2 1 0) "a") (make-worm 1 2 1 0))
(check-expect (handle-keys (make-worm 3 4 0 1) " ") (make-worm 3 4 0 1))

(define (handle-keys w k)
  (cond
    [(key=? k "left") (make-worm (worm-x w) (worm-y w) -1 0)]
    [(key=? k "right") (make-worm (worm-x w) (worm-y w) 1 0)]
    [(key=? k "up") (make-worm (worm-x w) (worm-y w) 0 -1)]
    [(key=? k "down") (make-worm (worm-x w) (worm-y w) 0 1)]
    [else w]))

; Number -> Worm
; run the program with the given clock rate
(define (worm-main rate)
  (big-bang (make-worm 5 5 1 0)
    [to-draw render]
    [on-tick update rate]
    [on-key handle-keys]))
