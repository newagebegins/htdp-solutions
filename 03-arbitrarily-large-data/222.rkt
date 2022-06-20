;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |222|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10)  ; # of blocks, horizontally
(define HEIGHT 15) ; # of blocks, vertically
(define SIZE 10)   ; blocks are squares

(define WIDTH-PX (* WIDTH SIZE))
(define HEIGHT-PX (* HEIGHT SIZE))

(define MAX-X (- WIDTH 1))
(define MAX-Y (- HEIGHT 1))

; red squares with black rims
(define BLOCK
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))

(define MTS (empty-scene WIDTH-PX HEIGHT-PX))

(define-struct tetris [block landscape])
(define-struct block [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
; - (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)

; interpretations
; (make-block x y) depicts a block whose left corner is (* x SIZE) pixels from the left
; and (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the dropping block, while b1, b2, and ... are resting

; Block Image -> Image
; render a single block onto the given image
(check-expect (render-block (make-block 0 0) MTS)
              (place-image/align BLOCK 0 0 "left" "top" MTS))
(check-expect (render-block (make-block 3 2) MTS)
              (place-image/align BLOCK (* 3 SIZE) (* 2 SIZE) "left" "top" MTS))

(define (render-block b img)
  (place-image/align BLOCK (* (block-x b) SIZE) (* (block-y b) SIZE) "left" "top" img))

; Landscape Image -> Image
; render the landscape onto the given image
(check-expect (render-landscape '() MTS) MTS)
(check-expect (render-landscape (list (make-block 0 MAX-Y) (make-block 0 (- MAX-Y 1))
                                      (make-block 1 MAX-Y) (make-block MAX-X MAX-Y))
                                MTS)
              (place-image/align
               BLOCK (* 0 SIZE) (* MAX-Y SIZE) "left" "top"
               (place-image/align
                BLOCK (* 0 SIZE) (* (- MAX-Y 1) SIZE) "left" "top"
                (place-image/align
                 BLOCK (* 1 SIZE) (* MAX-Y SIZE) "left" "top"
                 (place-image/align
                  BLOCK (* MAX-X SIZE) (* MAX-Y SIZE) "left" "top"
                  MTS)))))

(define (render-landscape l img)
  (cond
    [(empty? l) img]
    [else (render-block (first l) (render-landscape (rest l) img))]))

; Tetris -> Image
; render the game
(check-expect (tetris-render (make-tetris (make-block 0 0) '()))
              (place-image/align BLOCK 0 0 "left" "top" MTS))
(check-expect (tetris-render (make-tetris (make-block 3 2) '()))
              (place-image/align BLOCK (* 3 SIZE) (* 2 SIZE) "left" "top" MTS))
(check-expect (tetris-render (make-tetris (make-block 0 0)
                                          (list (make-block 0 MAX-Y) (make-block 0 (- MAX-Y 1))
                                                (make-block 1 MAX-Y) (make-block MAX-X MAX-Y))))
              (place-image/align
               BLOCK 0 0 "left" "top"
               (place-image/align
                BLOCK (* 0 SIZE) (* MAX-Y SIZE) "left" "top"
                (place-image/align
                 BLOCK (* 0 SIZE) (* (- MAX-Y 1) SIZE) "left" "top"
                 (place-image/align
                  BLOCK (* 1 SIZE) (* MAX-Y SIZE) "left" "top"
                  (place-image/align
                   BLOCK (* MAX-X SIZE) (* MAX-Y SIZE) "left" "top" MTS))))))

(define (tetris-render t)
  (render-block (tetris-block t)
                (render-landscape (tetris-landscape t) MTS)))

; Block -> Block
; move the block one cell down
(check-expect (move-down (make-block 0 0)) (make-block 0 1))
(check-expect (move-down (make-block 3 (- MAX-Y 1))) (make-block 3 MAX-Y))

(define (move-down b)
  (make-block (block-x b) (add1 (block-y b))))

; Block -> Block
; move the block one cell left
(check-expect (move-left (make-block 1 0)) (make-block 0 0))
(check-expect (move-left (make-block MAX-X 5)) (make-block (- MAX-X 1) 5))

(define (move-left b)
  (make-block (sub1 (block-x b)) (block-y b)))

; Block -> Block
; move the block one cell left
(check-expect (move-right (make-block 0 0)) (make-block 1 0))
(check-expect (move-right (make-block 5 8)) (make-block 6 8))

(define (move-right b)
  (make-block (add1 (block-x b)) (block-y b)))

; Block -> Boolean
; return #true if the block has reached the floor
(check-expect (hit-floor? (make-block 0 (- MAX-Y 1))) #false)
(check-expect (hit-floor? (make-block 0 MAX-Y)) #true)
(check-expect (hit-floor? (make-block 1 MAX-Y)) #true)

(define (hit-floor? b)
  (= (block-y b) MAX-Y))

; Block Landscape -> Boolean
; return #true if the dropping block b has landed on top of another block in l
(check-expect (hit-block? (make-block 0 (- MAX-Y 1))
                          (list (make-block 1 MAX-Y)))
              #false)
(check-expect (hit-block? (make-block 0 (- MAX-Y 1))
                          (list (make-block 0 MAX-Y)))
              #true)
(check-expect (hit-block? (make-block 3 (- MAX-Y 2))
                          (list (make-block 3 (- MAX-Y 1)) (make-block 3 MAX-Y)))
              #true)

(define (hit-block? b l)
  (member? (move-down b) l))

; Tetris -> Boolean
; return #true if the dropping block has landed on the floor or on top of another block

; the block is still dropping
(check-expect (block-landed? (make-tetris (make-block 0 (- MAX-Y 1)) '()))
              #false)
(check-expect (block-landed? (make-tetris (make-block 0 (- MAX-Y 2))
                                          (list (make-block 0 MAX-Y))))
              #false)
(check-expect (block-landed? (make-tetris (make-block 0 (- MAX-Y 1))
                                          (list (make-block 1 MAX-Y))))
              #false)

; the block has landed on the floor
(check-expect (block-landed? (make-tetris (make-block 0 MAX-Y) '()))
              #true)
(check-expect (block-landed? (make-tetris (make-block 0 MAX-Y)
                                          (list (make-block 3 MAX-Y))))
              #true)

; the block has landed on another block
(check-expect (block-landed? (make-tetris (make-block 0 (- MAX-Y 1))
                                          (list (make-block 0 MAX-Y))))
              #true)
(check-expect (block-landed? (make-tetris (make-block 3 (- MAX-Y 2))
                                          (list (make-block 3 (- MAX-Y 1)) (make-block 3 MAX-Y))))
              #true)

(define (block-landed? t)
  (or (hit-floor? (tetris-block t))
      (hit-block? (tetris-block t) (tetris-landscape t))))

; Block -> Block
; create a new dropping block to the right of the given landed block
(check-expect (new-block (make-block 0 MAX-Y)) (make-block 1 0))
(check-expect (new-block (make-block 0 (- MAX-Y 1))) (make-block 1 0))
(check-expect (new-block (make-block 2 MAX-Y)) (make-block 3 0))
(check-expect (new-block (make-block MAX-X MAX-Y)) (make-block 0 0))

(define (new-block b)
  (make-block (if (> (add1 (block-x b)) MAX-X)
                  0
                  (add1 (block-x b)))
              0))

; Tetris -> Tetris
; update the game for one tick

; the block is falling
(check-expect (tetris-update (make-tetris (make-block 0 0) '()))
              (make-tetris (make-block 0 1) '()))
(check-expect (tetris-update (make-tetris (make-block 2 3) '()))
              (make-tetris (make-block 2 4) '()))

; the block has reached the bottom, empty landscape
(check-expect (tetris-update (make-tetris (make-block 3 MAX-Y) '()))
              (make-tetris (make-block 4 0) (list (make-block 3 MAX-Y))))
(check-expect (tetris-update (make-tetris (make-block MAX-X MAX-Y) '()))
              (make-tetris (make-block 0 0) (list (make-block MAX-X MAX-Y))))

; the block has reached the bottom, non-empty landscape
(check-expect (tetris-update (make-tetris (make-block 0 MAX-Y)
                                          (list (make-block 1 MAX-Y))))
              (make-tetris (make-block 1 0)
                           (list (make-block 0 MAX-Y) (make-block 1 MAX-Y))))
(check-expect (tetris-update (make-tetris (make-block 3 MAX-Y)
                                          (list (make-block 0 MAX-Y) (make-block 5 MAX-Y))))
              (make-tetris (make-block 4 0)
                           (list (make-block 3 MAX-Y) (make-block 0 MAX-Y) (make-block 5 MAX-Y))))

; the block has landed on another block
(check-expect (tetris-update (make-tetris (make-block 0 (- MAX-Y 1))
                                          (list (make-block 0 MAX-Y))))
              (make-tetris (make-block 1 0) (list (make-block 0 (- MAX-Y 1))
                                                  (make-block 0 MAX-Y))))
(check-expect (tetris-update (make-tetris (make-block 3 (- MAX-Y 2))
                                          (list (make-block 3 (- MAX-Y 1))
                                                (make-block 3 MAX-Y))))
              (make-tetris (make-block 4 0)
                           (list (make-block 3 (- MAX-Y 2))
                                 (make-block 3 (- MAX-Y 1))
                                 (make-block 3 MAX-Y))))

(define (tetris-update t)
  (if (block-landed? t)
      (make-tetris (new-block (tetris-block t))
                   (cons (tetris-block t) (tetris-landscape t)))
      (make-tetris (move-down (tetris-block t))
                   (tetris-landscape t))))

; Block Landscape -> Boolean
; return #true if block b is in a legal position

; legal
(check-expect (legal-move? (make-block 0 0) '()) #true)
(check-expect (legal-move? (make-block 0 0) (list (make-block 0 MAX-Y))) #true)
(check-expect (legal-move? (make-block MAX-X MAX-Y) '()) #true)

; outside the playfield, left
(check-expect (legal-move? (make-block -1 0) '()) #false)

; outside the playfield, right
(check-expect (legal-move? (make-block (+ MAX-X 1) 0) '()) #false)

; inside another block
(check-expect (legal-move? (make-block 0 (- MAX-Y 1))
                           (list (make-block 0 (- MAX-Y 1)) (make-block 0 MAX-Y)))
              #false)

(define (legal-move? b l)
  (not (or (< (block-x b) 0)
           (> (block-x b) MAX-X)
           (member? b l))))

; Tetris KeyEvent -> Tetris
; handle keypresses

; move left
(check-expect (tetris-keys (make-tetris (make-block 3 0) '()) "left")
              (make-tetris (make-block 2 0) '()))
; move left, hit left wall
(check-expect (tetris-keys (make-tetris (make-block 0 0) '()) "left")
              (make-tetris (make-block 0 0) '()))
; move right
(check-expect (tetris-keys (make-tetris (make-block 0 0) '()) "right")
              (make-tetris (make-block 1 0) '()))
; move right, hit right wall
(check-expect (tetris-keys (make-tetris (make-block MAX-X 0) '()) "right")
              (make-tetris (make-block MAX-X 0) '()))
; move left, hit another block
(check-expect (tetris-keys (make-tetris (make-block 1 (- MAX-Y 1))
                                        (list (make-block 0 (- MAX-Y 1)) (make-block 0 MAX-Y)))
                           "left")
              (make-tetris (make-block 1 (- MAX-Y 1))
                           (list (make-block 0 (- MAX-Y 1)) (make-block 0 MAX-Y))))
(check-expect (tetris-keys (make-tetris (make-block 1 (- MAX-Y 1))
                                        (list (make-block 5 MAX-Y) (make-block 0 (- MAX-Y 1)) (make-block 0 MAX-Y)))
                           "left")
              (make-tetris (make-block 1 (- MAX-Y 1))
                                        (list (make-block 5 MAX-Y) (make-block 0 (- MAX-Y 1)) (make-block 0 MAX-Y))))
; move right, hit another block
(check-expect (tetris-keys (make-tetris (make-block 0 (- MAX-Y 1))
                                        (list (make-block 1 (- MAX-Y 1)) (make-block 1 MAX-Y)))
                           "right")
              (make-tetris (make-block 0 (- MAX-Y 1))
                           (list (make-block 1 (- MAX-Y 1)) (make-block 1 MAX-Y))))

; ignore other keys
(check-expect (tetris-keys (make-tetris (make-block 3 5) '()) "a")
              (make-tetris (make-block 3 5) '()))

(define (tetris-keys t ke)
  (cond
    [(key=? ke "left") (if (legal-move? (move-left (tetris-block t)) (tetris-landscape t))
                           (make-tetris (move-left (tetris-block t)) (tetris-landscape t))
                           t)]
    [(key=? ke "right") (if (legal-move? (move-right (tetris-block t)) (tetris-landscape t))
                            (make-tetris (move-right (tetris-block t)) (tetris-landscape t))
                            t)]
    [else t]))

; Number -> Tetris
; run the game with the given clock rate: (tetris-main 0.1)
(define (tetris-main rate)
  (big-bang (make-tetris (make-block 0 0) '())
    [to-draw tetris-render]
    [on-tick tetris-update rate]
    [on-key tetris-keys]))
