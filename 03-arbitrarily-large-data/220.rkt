;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |220|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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
